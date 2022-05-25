#include <iostream>
#include <assert.h>
#include <cstring>
 
using namespace std;
 
// Global variables defining starting state
int x[6] = {2, 0, 4, 1, 5, 6};
 
void perm_to_code_forward(int x[6]);
void perm_to_code_reverse(int x[6]);
 
 
void perm_to_code_forward(int x[6])
{
    int k = (sizeof(x) / sizeof(int));
    while(k != 0)
    {
        k -= 1;
        int j = 0;
        while(j != k)
        {
            if ((x[j]) > (x[k]))
            {
                x[j] -= 1;
                assert((x[j]) >= (x[k]));
            }
            j += 1;
            assert(!(j == 0));
        }
        assert(!(k == (sizeof(x) / sizeof(int))));
    }
}
 
void perm_to_code_reverse(int x[6])
{
    int k = 0;
    while(k != (sizeof(x) / sizeof(int)))
    {
        int j = k;
        while(j != 0)
        {
            j -= 1;
            if ((x[j]) >= (x[k]))
            {
                x[j] += 1;
                assert((x[j]) > (x[k]));
            }
            assert(!(j == k));
        }
        k += 1;
        assert(!(k == 0));
    }
}
 
int main()
{
    
    perm_to_code_forward(x);
    cout << x << endl;
}
