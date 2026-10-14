/* The file server of miso serve and miso dev (serve.c). */
int miso_serve_start(const char *dir, int port, int dev);
void miso_serve_poll(int ms);
void miso_serve_reload(void);
long miso_mtime(const char *path);
long miso_spawn(const char *cmd);
int miso_child_done(long pid);
long miso_now_ms(void);
void miso_quit_on_ctrl_c(void);
