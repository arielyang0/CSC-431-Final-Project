#include <stdio.h>
#include <iostream>
#include <vector>
#include <cmath>
#include <string>
#include <algorithm>
using namespace std;

class Matrix {
public:
  int rows;
  int cols;
  vector<float> data;
  Matrix();//default constructor
  Matrix(const Matrix&);//copy constructor

  Matrix(int rows, int cols, float x = 0.0) {
    this->rows=rows;
    this->cols=cols;
    this->data.resize(rows*cols);
    for(int r=0; r<rows; r++)
      for(int c=0; c<cols; c++)
  this->data[r*cols+c]=x;
  }
  float operator()(int i, int j) const {
    return data[i*cols+j];
  }
  float &operator()(int i, int j) {
    return data[i*cols+j];
  }

  float getitem(int i, int j){
	return data[i*cols+j];
  }

  void setitem(int i, int j, float c){
	 data[i*cols+j]=c;
}

  Matrix identity(int rows, float c,float x);

};

Matrix::Matrix():rows(0),cols(0)//default constructor
{ /*Empty*/}

Matrix::Matrix(const Matrix& temp){//copy constructor
	this->rows=temp.rows;
	this->cols=temp.cols;
	this->data.resize(rows*cols);
    for(int r=0; r<rows; r++)
      for(int c=0; c<cols; c++)
		  this->data[r*cols+c]=temp(r,c);
}

ostream &operator<<(ostream &out, const Matrix& A) {
  out << "[";
  for(int r=0; r<A.rows; r++) {
    if(r>0) out << ",";
    out << "[";
    for(int c=0; c<A.cols; c++) {
      if(c>0) out << ",";
      out << A(r,c);    
    }
    out << "]"<<endl;
  }
  out << "]"<<endl;    
  return out;
}

Matrix input ()
{
	int r, c;
	cout << "enter number of rows: ";
	cin >> r;
	cout << "enter number of cols: ";
	cin >> c;
	Matrix A(r,c);
	for (int i=0; i<r; i++)
	{
		for (int j=0; j<c;j++)
		{
			cout << "enter A("
			     << i
				 <<","
				 <<j
				 <<"): ";
			cin >> A(i,j);
		}
	}
	return A;
}

Matrix operator+(const Matrix &A, const Matrix &B) {
  if(A.rows!=B.rows || A.cols!=B.cols)
    cout << "BAD\n";
  Matrix C(A.rows,A.cols);
  for(int r=0; r<A.rows; r++)
    for(int c=0; c<A.cols; c++)
      C(r,c)=A(r,c)+B(r,c);
  return C;
}

Matrix operator-(const Matrix &A, const Matrix &B) {
  if(A.rows!=B.rows || A.cols!=B.cols)
    cout << "BAD\n";
  Matrix C(A.rows,A.cols);
  for(int r=0; r<A.rows; r++)
    for(int c=0; c<A.cols; c++)
      C(r,c)=A(r,c)-B(r,c);
  return C;
}

Matrix operator*(float a, const Matrix &B) {
  Matrix C(B.rows,B.cols);
  for(int r=0; r<B.rows; r++)
    for(int c=0; c<B.cols; c++)
      C(r,c)=a*B(r,c);
  return C;
}

Matrix operator*(const Matrix &A, const Matrix &B) {
  if(A.cols!=B.rows)
    cout << "BAD\n";
  Matrix C(A.rows,B.cols);
  for(int r=0; r<A.rows; r++)
    for(int c=0; c<B.cols; c++)
      for(int k=0; k<A.cols; k++)
	C(r,c)+=A(r,k)*B(k,c);
  return C;
}

void swap(float&a, float &b) {
  float c=a; a=b; b=c;
}

Matrix transpose(Matrix A){
	Matrix B(A.cols,A.rows);
	for (int i=0; i<A.rows; i++)
		for (int j=0; j<A.cols; j++)
			B(j,i)=A(i,j);
	return B;
}

Matrix inv(Matrix A) {
  if(A.cols!=A.rows)
    cout << "BAD\n";
  Matrix B(A.rows,A.cols);
  float p;
  float q;
  int m;
  for(int r=0; r<B.cols;r++) B(r,r)=1;
  for(int c=0; c<A.cols;c++) {    
    m=c; p=A(c,c);
    for(int i=c+1; i<A.rows; i++)
      if(abs(A(i,c)) > abs(p)) {m=i; p=A(i,c);}
    for(int i=0; i<A.cols; i++) {
      swap(A(m,i),A(c,i));
      swap(B(m,i),B(c,i));
    }
    for(int i=0; i<A.cols; i++) {
      A(c,i) /= p; 
      B(c,i) /= p;
    }
    for(int r=0; r<A.rows; r++) 
      if(r!=c) {
	q = A(r,c);
	for(int i=0; i<A.cols; i++) {
	  A(r,i)-=q*A(c,i);
	  B(r,i)-=q*B(c,i);
	}
      }
  }
  return B;
}

Matrix operator/(float x, const Matrix &A)
{
	Matrix C = inv(A);
	for (int i=0; i<C.rows; i++)
		for(int j=0;j<C.cols; j++)
			C(i,j) = x * C(i,j);
	return C;
}

Matrix operator/(const Matrix &A, const Matrix &B)
{
	Matrix C = inv(B);
	if(A.cols != C.rows)
		cout << "incompatible dimensions" << endl;
	return A*C;
}

Matrix Matrix::identity(int r, float c,float x){
	Matrix A(r,r);
    for(int i=0; i<r;i++) 
        for(int j=0; j<r; j++){
			A(i,j)=x;
			A(i,i)=c;	
		}
	return A;
}

 Matrix row(Matrix A,int n){
	 Matrix B(1,A.cols);
	 for (int j=0; j<A.cols; j++)
		B(0,j)=A(n-1,j);	
	 return B;
 }

 Matrix col(Matrix A,int n){
	 Matrix B(1,A.rows);
	 for (int i=0; i<A.cols; i++)
		B(0,i)=A(i,n-1);	
	 return B;
 }

 
 bool is_almost_symmetric(Matrix A){
	 double ap=1e-6, rp=1e-4;
	 float max=0.0;
	 if (A.rows!=A.cols) return false;
	 else{
		 for (int r=0;r<A.rows-1;r++)
			 for (int c=0;c<A.cols;c++){
				 float delta = abs(A(r,c)-A(c,r));
			     if(delta >ap && delta > (max=(abs(A(r,c))>abs(A(c,r)) ? abs(A(r,c)):abs(A(c,r))))*rp)
					 return false;
			 }
	 }
	 return true;
 }

 bool is_almost_zero(Matrix A){
	 double ap=1e-6, rp=1e-4;
	 float max;
	 for (int r=0;r<A.rows;r++)
			 for (int c=0;c<A.cols;c++){
				 float delta = abs(A(r,c)-A(c,r));
				 if(delta>ap && delta>(max=(abs(A(r,c))>abs(A(c,r)) ? abs(A(r,c)):abs(A(c,r))))*rp)
					 return false;
			 }
	return true;
 }
 
float norm(Matrix A, int p=1){
	 float sum=0.0,max=0.0;
	 if (A.rows==1 || A.cols==1) {
		 for( int r=0;r<A.rows;r++)
			 for (int c=0;c<A.cols;c++)
				 sum += abs(A(r,c));
		 return sum;
	 }

	 else if(p==1){
		 float *number;
	     number = new float[A.cols];
		 for(int c=0;c<A.cols;c++){
			 sum=0.0;
			 for( int r=0;r<A.rows;r++){
				 sum +=abs(A(r,c));
			 }
			    number[c]=sum;
		 }

		 for (int i=0;i<A.cols;i++){
			 if(max<number[i]) max=number[i];
		 }
		 return max;
		 delete [] number;
	 }
 }

 
 float condition_number(Matrix A){
	 return norm(A)*norm(inv(A));
 }


 Matrix exp_m(Matrix A){
	float ap=1e-6, rp=1e-4,max;
	int ns=40;
	Matrix t,s;
	t=s=A.identity(A.cols,1.0,0.0);
	
	for(int i=1; i<ns; i++){
		t = (1/i)*t*A;
		s = s + t;
		if (norm(t)<(max=(ap>norm(s)*rp) ? ap : norm(s)*rp))
			return s;
	}
	cout<<"no convergence";
}
 Matrix Cholesky(Matrix A)
{
	 double p;
	 Matrix L;
	 if(is_almost_symmetric(A)){
			Matrix L=A;
			for(int k=0; k<L.cols; k++){
				if (L(k,k)<=0)
					cout<<"Arithmetic Error/";
					p=sqrt(L(k,k));
					L(k,k)=sqrt(L(k,k));
					for(int i=k+1; i<L.rows; i++)
						L(i,k)=L(i,k)/p;
					for(int j=k+1; j<L.rows; j++){
						p=float(L(j,k));
						for(int i=k+1; i<L.rows; i++)
							L(i,j)=L(i,j)-(p*L(i,k));}}
			for(int i=0; i<L.rows; i++){
				for(int j=i+1; j<L.cols; j++)
					L(i,j)=0;}
				return L;
						
			
	 }
	 else
		 cout<<"Arithmetic Error/";
}



bool is_positive_definite(Matrix A)
{
	try{
		 (Cholesky(A));
		 cout<<"Is positive definite"<<endl;
	}
	catch( ...){
		 cout<<"Is not positive definite"<<endl;
	}
	return 0;
}


Matrix Markowitz(Matrix mu, Matrix cov, float rfree)
{
	Matrix X(cov.rows,1);
	Matrix R(1,mu.cols);
	for(int i=0; i<mu.cols; i++){
		R(0,i)=0.05;}
	X=(mu-R)*(1/cov);
	double sum=0;
	for(int i=0; i<X.cols; i++)
		for(int j=0; j<X.rows; j++){
			sum=sum+X(j,i);}
	X=(1/sum)*X;
	Matrix Return(1,1);
	Matrix Risk(1,1);
	Matrix Y(X.cols, X.rows);
	for(int i=0; i<X.cols; i++){
		for(int j=0; j<X.rows; j++)
			Y(i,j)=X(j,i);}
	cout<<"Portfolio "<<X<<endl;
	Return = mu*Y;
	Risk = X*cov;
	Risk = Risk*Y;
	double Portfolio_Risk=sqrt(Risk(0,0));
	cout<< "Return " <<Return<<"  Risk "<<Portfolio_Risk<<endl;
	return X;
}


double sqroot(double a)
{
double x;
if (a<=0)
	cout << "BAD\n";
	x=sqrt(a);
	return (x);
}

class Function {
public:
  virtual double f(double x)=0;
  
  double Df(double x, double h=0.00001) {
    return (f(x+h)-f(x-h))/(2.0*h);
  }

  double DDf(double x, double h=1e-5) {
    return (f(x+h)-2.0*f(x)+f(x-h))/(h*h);
  }

  double condition_number(double x){
    return Df(x)*x/f(x);
  }
  
  double solve_newton(double x_guess, double ap=1e-5, double rp=1e-4, int ns=100) {
    double x_old, x = x_guess;
    for(int k=0; k<ns; k++) {
      x_old = x;
      x = x - f(x)/Df(x);
      if(abs(x-x_old)<max(ap,rp*abs(x))) return x;
    }
    throw string("no convergence");
  }  

  double solve_bisection(double a, double b, double ap=1e-6, double rp=1e-4, int ns=11){
	  double fa=f(a);
	  double fb=f(b); 
	  if (fa==0) 
		  return a;
	  if (fb==0) 
		  return b;
	  if (fa*fb>0) 
		  throw string ("fa and fb should have opposite signs");
	  for(int k=0;k<ns;k++){
		  double x=(a+b)/2;
		  double fx=f(x);
		  if (fx==0 || abs(b-a)<max(ap,abs(x)*rp))
			  return x;
		  else if (fx*fa<0)
		  {
			  b=x;
			  fa=fx; 
		  }
		  else {
			a =x;
		    fa=fx;	  
		  }
	  }
	  throw string ("no convergence");
  }
double solve_secant(double x, double ap=1e-6, double rp=1e-4, int ns=20){
		double fx=f(x);
		double dfx=Df(x);
		for(int k=0; k<ns;k++){
			if (abs(dfx) < ap) 
				throw string ("unstable solution");
			double x_old=x;
			double fx_old=fx;
			x=x-fx/dfx;
			if (k>2 && abs(x-x_old)<max(ap,abs(x)*rp)) 
				return x;
			fx=f(x);
			dfx=(fx-fx_old)/(x-x_old);
		} 
		throw string ("no convergence");
	}

double solve_newton_stabilized(double a, double b, double ap=1e-6, double rp=1e-4, int ns=20){
	  double fa=f(a);
	  double fb=f(b); 
	  if (fa==0) 
		  return a;
	  if (fb==0) 
		  return b;
	  if (fa*fb>0) 
		  throw string("no convergence");
	  double x=(a+b)/2;
	  double fx=f(x);
	  double dfx=Df(x);
	  for(int k=0;k<ns; k++){
		  double x_old=x;
		  double fx_old=fx;
		  if (abs(dfx)>ap)
			  x=x-fx/dfx;
		  if (x==x_old || x<a|| x>b)
			  x=(a+b)/2;
		  fx=f(x);
		  if(x==x_old || abs(x-x_old) <max(ap,abs(x)*rp)) return x;
		  dfx=(fx-fx_old)/(x-x_old);
		  if (fx*fa<0){ 
			  b=x;
			  fb=fx;
		  }
		  else {
			  a=x;
			  fa=fx;
		  }
	  throw string("no convergence");
	  }
}

double g(double x){return (f(x) +x);}
double solve_fixed_point(double x, double ap=1e-6, double rp=1e-4, int ns=100){
		double h = 0.00001;
		double dg=(g(x+h)-g(x-h))/(2.0*h);
		for(int k=0; k<ns;k++){
			if (abs(dg)>=1) 
				throw string("error Dg(x) >=1"); 
			double x_old= x;
			x=g(x); 
			if (k>2 && abs(x_old-x)<(max(ap,abs(x)*rp)))
				return x;
		}
		throw string("no convergence");
}

double optimize_newton(double x_guess, double ap=1e-5, 
			double rp=1e-4, int ns=20) {
    double x_old, x = x_guess;
    for(int k=0; k<ns; k++) {
      x_old = x;
      x = x - Df(x)/DDf(x);
      if(abs(x-x_old)<max(ap,rp*abs(x))) return x;
    }
    throw string("no convergence");
  }  

 double optimize_bisection(double a,double b,double ap=1e-6,double rp=1e-4,int ns=100)
 {
	 double Dfa = Df(a);
	 double Dfb = Df(b);
	 if (Dfa == 0)
		 return a;
	 if (Dfb == 0)
		 return b;
	 if (Dfa*Dfb > 0)
		 throw string ("Dfa and Dfb must have opposite signs");
	 for (int k=0; k<ns;k++)
	 {
		 double x =(a+b)/2;
		 double Dfx = Df(x);
		 if (Dfx == 0 || abs(b-a) < max(ap,abs(x))*rp)
			 return x;
		 else if (Dfx * Dfa < 0)
		 {
			 b = x;
			 Dfb = Dfx;
		 }
		 else
		 {
			 a = x;
			 Dfa = Dfx;
		 }
	 }
	 throw string ("no convergence");
 }

 double optimize_secant (double x, double ap=1e-6,double rp=1e-4,int ns=100)
 {
	 double fx = f(x);
	 double Dfx = Df(x);
	 double DDfx = DDf(x);
	 for (int k=0;k<ns;k++)
	 {
		 if (Dfx==0)
			 return x;
		 if (abs(DDfx)<ap)
			 throw string ("unstable solution");
		 double x_old = x;
		 double Dfx_old = Dfx;
		 x = x - Dfx / DDfx;
		 if (abs(x-x_old)<max(ap, abs(x)*rp))
			 return x;
		 fx = f(x);
		 Dfx = Df(x);
		 DDfx = (Dfx - Dfx_old)/(x - x_old);
	 }
	 throw string ("no convergence");
 }

 double optimize_newton_stabilized(double a,double b,double ap=1e-6,double rp=1e-4,int ns=20)
 {
	 double Dfa = Df(a);
	 double Dfb = Df(b);
	 if (Dfa ==0)
		 return a;
	 if (Dfb == 0)
		 return b;
	 if (Dfa*Dfb>0)
		 throw string ("Dfa and Dfb must have opposite signs");
	 double x = (a+b)/2;
	 double fx = f(x);
	 double Dfx = Df(x);
	 double DDfx = DDf(x);
	 for (int k=0;k<ns;k++)
	 {
		 if (Dfx == 0)
			 return x;
		 double x_old = x;
		 double fx_old = fx;
		 double Dfx_old = Dfx;
		 if (abs(DDfx)>ap)
			 x = x - Dfx/DDfx;
		 if (x==x_old || x<a || x>b)
			 x = (a+b)/2;
		 if (abs(x-x_old)<max(ap,abs(x)*rp))
			 return x;
		 fx = f(x);
		 Dfx = (fx-fx_old)/(x-x_old);
		 DDfx = (Dfx-Dfx_old)/(x-x_old);
		 if (Dfx*Dfa<0)
		 {
			 b = x;
			 Dfb = Dfx;
		 }
		 else
		 {
			 a = x;
			 Dfa = Dfx;
		 }
	 }
	 throw string ("no convergence");
 }

 double optimize_golden_search(double a,double b,double ap=1e-6,double rp=1e-4,int ns=100)
 {
	 double tau = (sqrt(5.0)-1.0)/2.0;
	 double x1 = a+(1.0-tau)*(b-a);
	 double x2 = a+tau*(b-a);
	 double fa = f(a);
	 double f1 = f(x1);
	 double f2 = f(x2);
	 double fb = f(b);
	 for (int k=0;k<100;k++)
	 {
		 if (f1>f2)
		 {
			 a = x1;
			 fa = f1;
			 x1 = x2;
			 f1 = f2;
			 x2 = a+tau*(b-a);
			 f2 = f(x2);
		 }
		 else 
		 {
			 b = x2;
			 fb = f2;
			 x2 = x1;
			 f2 = f1;
			 x1 = a+(1.0-tau)*(b-a);
			 f1 = f(x1);
		 }
		 if (k>2 && abs(b-a)<max(ap,abs(b)*rp))
			 return b;
	 }
	 throw string ("no convergence");
 }
};


class MyFunction : public Function {
  double f(double x) { return (x-2)*(x+8); }
};

class MyOtherFunction : public Function {
  double f(double x) { return (x-1)*(x+3); }
};

class SimpleTransaction {
public:
  double A, t;
  SimpleTransaction(double A, double t) {
    this->A = A;
    this->t = t;
  }
  double present_value(float r) {
    return A*exp(-r*t);
  }
};

class ComplexTransaction {
public:
  vector<SimpleTransaction> transactions;
  double present_value(float r) {
    double total = 0;
    for(int k=0; k<transactions.size(); k++) {
      total += transactions[k].present_value(r);
    }
    return total;
  }
};

class MyBondFunction : public Function {
public:
  double A, t, p;
  int n;
  double f(double r) {
    ComplexTransaction bond;
    for(int i=1; i<=n; i++)
      bond.transactions.push_back(SimpleTransaction(A,t*i));
    return bond.present_value(r)-p;
  }
};

class Portfolio2 : public Function {
public:
  double r1, r2, sigma1, sigma2, rho, r_free;
  double R(double x) {
    return x*r1+(1.0-x)*r2;
  }
  double sigma(double x) {
    return sqrt(x*x*sigma1*sigma1+2.0*x*(1.0-x)*sigma1*sigma2*rho+(1.0-x)*(1.0-x)*sigma2*sigma2);
  }
  double sharpe(double x) {
    return (R(x)-r_free)/sigma(x);
  }
  double f(double x) {
    return sharpe(x);
  }
};

int main() {
  Matrix A = input();
  cout<<A;
  cout<<is_almost_symmetric(A)<<endl;
  cout<<is_almost_zero(A)<<endl;
  cout<<condition_number(A)<<endl;
  cout<<exp_m(A)<<endl;
  cout<<norm(A)<<endl;
  A.setitem(1,0,10);
  cout << A;
  Matrix C;
  cout<<C.identity(4,2,1);
  cout<<A;
  cout<<row(C.identity(4,2,1),2);
  cout<<col(C.identity(4,2,1),1);
  Matrix B(A.rows,A.cols);
  B = inv(A);
  cout << B*A << endl;
  cout << transpose(C) << endl;
  cout << A/3 <<endl;
  cout << A/A;
  Matrix C(3,3);
  cout<< C.identity(3,1,0);
  
  
  MyFunction a;

  cout << a.condition_number(0.0)<<endl;

  cout << a.optimize_newton(-2.0) << endl;
  cout << a.optimize_bisection(-2.5,-3.5) << endl;
  cout << a.optimize_secant(-2.0) << endl;
  cout << a.optimize_newton_stabilized(-2.5,-3.5) << endl;
  cout << a.optimize_golden_search(-2.5,-3.5) << endl;
  
  cout << a.solve_newton(10)<< endl;
  cout << a.solve_bisection(1.5, 2.5)<< endl; 
  cout << a.solve_newton_stabilized(1.5,2.5)<< endl;
  cout << a.solve_secant(1.5) << endl;
  /*cout << a.solve_fixed_point(1.5)<<endl;*/
  system("pause");
  return 0;
}