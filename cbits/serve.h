/* The file server of miso serve and miso dev (serve.c). */
int miso_serve_start(const char *dir, int port, int dev);
void miso_serve_poll(int ms);
void miso_serve_reload(void);
long miso_mtime(const char *path);
