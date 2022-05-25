#include <assert.h>
#include <cstring>
 
using namespace std;
 
// Global variables defining starting state
int x1 = 0;
int x2 = 1;
 
void fib_forward(int &x1, int &x2, const int n);
void fib_reverse(int &x1, int &x2, const int n);
 
 
void fib_forward(int &x1, int &x2, const int n)
{
    int i = 1;
    while(i != n)
    {
        int tmp = (x2) - (x1);
        x2 += x1;
        x1 += tmp;
        assert(tmp == (x1) - ((x2) - (x1)));
        i += 1;
        assert(!(i == 1));
    }
}
 
void fib_reverse(int &x1, int &x2, const int n)
{
    int i = n;
    while(i != 1)
    {
        i -= 1;
        int tmp = (x1) - ((x2) - (x1));
        x1 -= tmp;
        x2 -= x1;
        assert(tmp == (x2) - (x1));
        assert(!(i == n));
    }
}
 
int main()
{
    
    
    int i = 0;
    while(i != 100000)
    {
        fib_forward(x1, x2, 2000);
        fib_reverse(x1, x2, 2000);
        i += 1;
        assert(!(i == 0));
    }
    assert(((x1) == (0)) && ((x2) == (1)));
}
