#include <iostream>
#include <math.h>

using namespace std;

double F(double x,double E, double PI);
int biseccion(double a, double b, double TOL, double MAX_IT);

int main()
{
    int cantidad;
    cantidad = biseccion(2,-2,pow(10,-8),50);
    cout<<"La cantidad de iteraciones fueron de: "<<cantidad<<endl;
    return 0;
}
double F(double x,double E, double PI){
    return (x*pow(E,x))-PI;
}
int biseccion(double a, double b, double TOL, double MAX_IT){
    int iteraciones = 0;
    double c;
    
    while(iteraciones < MAX_IT){
       c = (a+b)/2;
       if(fabs(F(c,2.71828,3.1415))<TOL){
           break;
       }
       if(F(a,2.71828,3.1415)*F(c,2.71828,3.1415)<0){
           b = c;
       }
       if(F(b,2.71828,3.1415)*F(c,2.71828,3.1415)<0){
           a = c;
       }
       
       iteraciones++;
    }
    cout<<"La raiz buscada es: "<<c<<endl;
    return iteraciones;
}
