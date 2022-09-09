/* Generated C code and the driver */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

void rll(const int64_t * aa_1,const int64_t al_2);

int main(void) {
  int64_t arr[] = {41,41,41,41,42,42,42,43,43,41};
  rll(arr,sizeof(arr)/sizeof(arr[0]));
  printf("Done\n");
}

/* Generated code */
void rll(const int64_t * aa_1,const int64_t al_2)
{
   int64_t v_3 = 0;
   int64_t v_4 = 0;
   bool v_5 = 0;
   int64_t v_6 = 0;
   while (v_6 <= al_2)
   {
      int64_t t_7;
      t_7 = v_6;
      v_6++;
      if (t_7 < al_2)
      {
         int64_t t_9;
         t_9 = aa_1[t_7];
         if (v_5)
         {
            int64_t t_10;
            t_10 = v_4;
            v_4 = t_9;
            if (!(t_10 == t_9))
            {
               int64_t t_11;
               t_11 = v_3;
               v_3 = 0;
               printf("%ld\n",(long)t_10);
               printf("%ld\n",(long)(t_11 + 1));
            }
            else {
                    v_3++;
            }
         }
         else {
                 v_4 = t_9;
                 v_5 = 1;
         }
      }
      else {
              if (v_5)
              {
                 int64_t t_8;
                 t_8 = v_3;
                 v_3 = 0;
                 printf("%ld\n",(long)v_4);
                 printf("%ld\n",(long)(t_8 + 1));
              }
      }
   }}
