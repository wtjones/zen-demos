#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>

#define MAX_CHILDREN 3

int main()
{
    pid_t child[MAX_CHILDREN];
    int status[MAX_CHILDREN];
    int fds[2];
    int r = pipe(fds);
    if (r == -1) {
        return 0;
    }

    printf("Hello World!\n");
    printf("I'm parent with pid %d: \n", getpid());
    pid_t pid;
    char buf[100];
    if (!fork()) {
        printf("L\n");
        printf("I'm child 1 with pid %d: \n", getpid());
        sleep(1);
        write(fds[1], "From child 1", 12);
        _exit(0);
    } else if (!fork()) {
        printf("L\n");
        printf("I'm child 2 with pid %d: \n", getpid());
        sleep(2);
        write(fds[1], "From child 2", 12);
        sleep(1);
        write(fds[1], "From child 2", 12);

        _exit(0);
    } else if (!fork()) {
        printf("I'm child 3 with pid %d: \n", getpid());
        sleep(3);
        write(fds[1], "From child 3", 12);
        _exit(0);
    } else {
        // Don't block on reads
        int retval = fcntl(fds[0], F_SETFL, fcntl(fds[0], F_GETFL) | O_NONBLOCK);
        printf("Ret from fcntl: %d\n", retval);

        while (1) {
            ssize_t r = read(fds[0], buf, 12);
            printf("read: %zd\n", r);

            if (r > 0) {
                printf("Buffer: %s\n", buf);
            } else {
                printf("Read nothing\n");
                perror("Error was");
                sleep(1);
            }
        }
    }

    sleep(1);

    return 0;
}
