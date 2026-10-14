/*
 * A small static file server for miso serve and miso dev.
 *
 * Single threaded: miso calls miso_serve_poll, which handles what is ready
 * (with select) and returns, so the caller can watch the sources between
 * polls.  In dev mode HTML pages get a script that listens on
 * /__miso/events (server sent events), and miso_serve_reload tells those
 * pages to reload.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "serve.h"

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
typedef SOCKET sock_t;
#define CLOSE closesocket
#define BAD_SOCK INVALID_SOCKET
#else
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>
#include <arpa/inet.h>
typedef int sock_t;
#define CLOSE close
#define BAD_SOCK (-1)
#endif

#define MAX_EVENT_CLIENTS 64

static sock_t listener = BAD_SOCK;
static char root[1024];
static int dev_mode;
static sock_t events[MAX_EVENT_CLIENTS];
static int nevents;

static const char reload_script[] =
  "<script>new EventSource('/__miso/events').onmessage = function () { location.reload(); };</script>";

static const char *mime(const char *path) {
  const char *dot = strrchr(path, '.');
  if (!dot) return "application/octet-stream";
  if (!strcmp(dot, ".html")) return "text/html; charset=utf-8";
  if (!strcmp(dot, ".js") || !strcmp(dot, ".mjs")) return "text/javascript; charset=utf-8";
  if (!strcmp(dot, ".css")) return "text/css; charset=utf-8";
  if (!strcmp(dot, ".wasm")) return "application/wasm";
  if (!strcmp(dot, ".json")) return "application/json";
  if (!strcmp(dot, ".svg")) return "image/svg+xml";
  if (!strcmp(dot, ".png")) return "image/png";
  if (!strcmp(dot, ".jpg") || !strcmp(dot, ".jpeg")) return "image/jpeg";
  if (!strcmp(dot, ".gif")) return "image/gif";
  if (!strcmp(dot, ".ico")) return "image/x-icon";
  if (!strcmp(dot, ".txt")) return "text/plain; charset=utf-8";
  return "application/octet-stream";
}

static void send_all(sock_t s, const char *buf, size_t n) {
  while (n > 0) {
    int k = send(s, buf, (int)n, 0);
    if (k <= 0) return;
    buf += k;
    n -= (size_t)k;
  }
}

static void respond(sock_t s, const char *status, const char *type, const char *body, size_t len) {
  char head[512];
  int h = snprintf(head, sizeof head,
                   "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %lu\r\n"
                   "Cache-Control: no-cache\r\nConnection: close\r\n\r\n",
                   status, type, (unsigned long)len);
  send_all(s, head, (size_t)h);
  if (body) send_all(s, body, len);
}

static char *read_file(const char *path, size_t *len) {
  FILE *f = fopen(path, "rb");
  char *buf;
  long n;
  if (!f) return NULL;
  fseek(f, 0, SEEK_END);
  n = ftell(f);
  fseek(f, 0, SEEK_SET);
  buf = malloc((size_t)n + sizeof reload_script + 1);
  if (buf && fread(buf, 1, (size_t)n, f) != (size_t)n) { free(buf); buf = NULL; }
  fclose(f);
  *len = (size_t)n;
  return buf;
}

/* Put the reload script before </body>, or at the end. */
static void inject(char *buf, size_t *len) {
  size_t sl = sizeof reload_script - 1, at = *len, i;
  buf[*len] = 0;
  for (i = 0; i + 7 <= *len; i++)
    if (!strncmp(buf + i, "</body>", 7) || !strncmp(buf + i, "</BODY>", 7)) { at = i; break; }
  memmove(buf + at + sl, buf + at, *len - at);
  memcpy(buf + at, reload_script, sl);
  *len += sl;
}

static int is_dir(const char *p) {
  struct stat st;
  return stat(p, &st) == 0 && (st.st_mode & S_IFMT) == S_IFDIR;
}

static void handle(sock_t s) {
  char req[4096], path[2048], file[4096];
  int n = recv(s, req, sizeof req - 1, 0), i, j;
  char *buf;
  size_t len;
  if (n <= 0) { CLOSE(s); return; }
  req[n] = 0;
  if (strncmp(req, "GET ", 4) && strncmp(req, "HEAD ", 5)) {
    respond(s, "405 Method Not Allowed", "text/plain", "method not allowed\n", 19);
    CLOSE(s);
    return;
  }
  /* the path, without the query, %-decoded */
  for (i = strchr(req, ' ') - req + 1, j = 0; req[i] && req[i] != ' ' && req[i] != '?' && j < (int)sizeof path - 1; i++) {
    if (req[i] == '%' && req[i + 1] && req[i + 2]) {
      char hex[3] = { req[i + 1], req[i + 2], 0 };
      path[j++] = (char)strtol(hex, NULL, 16);
      i += 2;
    } else {
      path[j++] = req[i];
    }
  }
  path[j] = 0;

  if (dev_mode && !strcmp(path, "/__miso/events")) {
    const char *head = "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n"
                       "Cache-Control: no-cache\r\nConnection: keep-alive\r\n\r\n";
    send_all(s, head, strlen(head));
    if (nevents < MAX_EVENT_CLIENTS) events[nevents++] = s; else CLOSE(s);
    return;
  }
  if (strstr(path, "..") || path[0] != '/') {
    respond(s, "403 Forbidden", "text/plain", "forbidden\n", 10);
    CLOSE(s);
    return;
  }
  snprintf(file, sizeof file, "%s%s", root, path);
  if (is_dir(file)) {
    size_t l = strlen(file);
    snprintf(file + l, sizeof file - l, "%sindex.html", file[l - 1] == '/' ? "" : "/");
  }
  buf = read_file(file, &len);
  if (!buf) {
    respond(s, "404 Not Found", "text/plain", "not found\n", 10);
  } else {
    const char *type = mime(file);
    if (dev_mode && !strncmp(type, "text/html", 9)) inject(buf, &len);
    if (!strncmp(req, "HEAD ", 5)) respond(s, "200 OK", type, NULL, len);
    else respond(s, "200 OK", type, buf, len);
    free(buf);
  }
  CLOSE(s);
}

/* Start serving dir on 127.0.0.1:port.  0 on success. */
int miso_serve_start(const char *dir, int port, int dev) {
  struct sockaddr_in addr;
  int one = 1;
#ifdef _WIN32
  WSADATA wsa;
  if (WSAStartup(MAKEWORD(2, 2), &wsa)) return -1;
#else
  signal(SIGPIPE, SIG_IGN);       /* a closed page must not stop the server */
#endif
  snprintf(root, sizeof root, "%s", dir);
  dev_mode = dev;
  listener = socket(AF_INET, SOCK_STREAM, 0);
  if (listener == BAD_SOCK) return -1;
  setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, (const char *)&one, sizeof one);
  memset(&addr, 0, sizeof addr);
  addr.sin_family = AF_INET;
  addr.sin_port = htons((unsigned short)port);
  addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  if (bind(listener, (struct sockaddr *)&addr, sizeof addr) || listen(listener, 32)) {
    CLOSE(listener);
    listener = BAD_SOCK;
    return -1;
  }
  return 0;
}

/* Handle the requests that come within ms milliseconds. */
void miso_serve_poll(int ms) {
  fd_set fds;
  struct timeval tv;
  if (listener == BAD_SOCK) return;
  FD_ZERO(&fds);
  FD_SET(listener, &fds);
  tv.tv_sec = ms / 1000;
  tv.tv_usec = (ms % 1000) * 1000;
  while (select((int)listener + 1, &fds, NULL, NULL, &tv) > 0) {
    sock_t s = accept(listener, NULL, NULL);
    if (s != BAD_SOCK) handle(s);
    FD_ZERO(&fds);
    FD_SET(listener, &fds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
  }
}

/* Tell the pages to reload. */
void miso_serve_reload(void) {
  static const char msg[] = "data: reload\n\n";
  int i;
  for (i = 0; i < nevents; i++) {
    send(events[i], msg, (int)sizeof msg - 1, 0);
    CLOSE(events[i]);
  }
  nevents = 0;
}

/* The modification time of a file (seconds), or -1. */
long miso_mtime(const char *path) {
  struct stat st;
  if (stat(path, &st)) return -1;
  return (long)st.st_mtime;
}
