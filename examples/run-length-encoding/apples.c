/* Generated C code and the driver */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

void rll(int64_t const n_1,int64_t * const a_2);

int main(void) {
  int64_t arr[] = {41,41,41,41,42,42,42,43,43,41};
  rll(sizeof(arr)/sizeof(arr[0]),arr);
  printf("Done\n");
}

/* Generated code */
void rll(int64_t const n_1,int64_t * const a_2){
  int64_t x_3 = 0;
  int64_t x_4 = 0;
  bool x_5 = false;
  int64_t x_6 = 0;
  while (x_6 <= n_1)
  {
    int64_t const t_7 = x_6;
    x_6++;
    if (t_7 < n_1)
    {
      int64_t const t_9 = a_2[t_7];
      if (x_5)
      {
        int64_t const t_10 = x_4;
        x_4 = t_9;
        if (!(t_10 == t_9))
        {
          int64_t const t_11 = x_3;
          x_3 = 0;
          printf("%ld\n",t_10);
          printf("%ld\n",t_11 + 1);
        }
        else 
          x_3++;
      }
      else {
        x_4 = t_9;
        x_5 = true;
      }
    }
    else {if (x_5)
            {
              int64_t const t_8 = x_3;
              x_3 = 0;
              printf("%ld\n",x_4);
              printf("%ld\n",t_8 + 1);
            }}
  }
}
