#include <iostream>
#include <vector>
#include <set>
using std::cout;
using std::cin;
using std::endl;
using std::vector;
using std::set;

const int MAXN = 80000;
const int SIEVE = 4000000;
const int SIEVESQRT = 2000;

int prime[MAXN] = {};
bool sieve[SIEVE] = {};
void primes()
{
        for(int i=2; i<SIEVESQRT; i++)
        {
                if (sieve[i] == true)
                        continue;
                for(int j=i*i; j<SIEVE; j+=i)
                        sieve[j] = true;

        }
        int j=0;
        for(int i=2; j<MAXN; i++)
        {
                if (sieve[i] == false)
                {
                        prime[j] = i;
                        j++;

                }

        }

}

int factor(int n)
{
        if (sieve[n] == false)
                return 1;
        for(int i=0; prime[i]<=n/2; i++)
                if (n%prime[i] == 0)
                        return n/prime[i];
        return 1;

}

const int MAXKN = 120;

int kn[MAXKN+1] = {};
int curri=0;
int yht=0;

vector<int> empties;

void iterate(vector<int>& base, vector<int> arrangement, int numero)
{
        empties.clear();
        for(int i=0; i<arrangement.size(); i++)
                if (arrangement.at(i) == 0)
                        empties.push_back(i);

        int isoinarr=0;
        for(int i=0; i<arrangement.size(); i++)
                if (arrangement[i] > isoinarr)
                        isoinarr = arrangement[i];

        if (empties.empty())
        {
                empties.assign(isoinarr+1, 1);
                for(int i=0; i<arrangement.size(); i++)
                {
                        empties[arrangement[i]] *= base[i];

                }
                int total=0;
                for(int i=1; i<empties.size(); i++)
                {
                        if (empties.at(i) > 1)
                                total+=empties.at(i);

                }
                int ultimatetotal = curri-total+isoinarr;
                if (ultimatetotal <= MAXKN && kn[ultimatetotal] == 0)
                {
                        kn[ultimatetotal] = curri;
                        yht++;

                }
                return;

        }
        int nollas = empties[0];
        for(int j=1; j<=isoinarr+1; j++)
        {
                arrangement[nollas] = j;
                iterate(base, arrangement, j);

        }

}

int main()
{
        primes();
        for(int i=2; i<30000; i++)
        {
                if (sieve[i] == false)
                        continue;
                vector<int> factorit;
                int j=i;
                while(j>1)
                {
                        factorit.push_back(j/factor(j));
                        j = factor(j);

                }
                vector<int> arr(factorit.size(), 0);
                arr[0] = 1;
                curri=i;
                iterate(factorit, arr, 1);
                if (yht == MAXKN)
                        break;

        }
        set<int> setti;
        for(int i=2; i<=MAXKN; i++)
                setti.insert(kn[i]);

        int summa=0;
        for(set<int>::iterator sit = setti.begin(); sit!=setti.end(); sit++)
                summa += (*sit);

        cout << summa << endl;

}
