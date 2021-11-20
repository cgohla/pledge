#include <unistd.h>  /*pledge*/
#include <stdio.h> /*printf*/
#include <string.h> /*strcmp*/
#include <fcntl.h> /*open*/

int main(int argc, char **argv){
  char *dummyFile = "/tmp/demo.dummy.foo";

  int fd;
  if(argc > 1) {
    if(!strcmp(argv[1],"--pledge")){ // strcmp returns 0 on equality, but that is boolean false.
      pledge("stdio",""); // From now on we will only use stdio capabilities.
      printf("pledge called\n");
    }
  }
  printf("hello world\n");
  printf("attempt to open %s.\nthis should fail when running with \"pledge\".\n", dummyFile);
  fd = open(dummyFile, O_RDWR);
  return 0;
}
