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



int main() {
  Matrix A = input();
  cout<<A;
  //cout<<is_almost_symmetric(A)<<endl;
  //cout<<is_almost_zero(A)<<endl;
  cout<<condition_number(A)<<endl;
  //cout<<exp_m(A)<<endl;
  //cout<<norm(A)<<endl;
  //A.setitem(1,0,10);
  // cout << A;
	//Matrix C;
	//cout<<C.identity(4,2,1);
	//cout<<A;
	//cout<<row(C.identity(4,2,1),2);
	//cout<<col(C.identity(4,2,1),1);
 // Matrix B(A.rows,A.cols);
  //B = inv(A);
  //cout << B*A << endl;
  //cout << transpose(C) << endl;
  //cout << A/3 <<endl;
  //cout << A/A;
 //Matrix C(3,3);
 // cout<< C.identity(3,1,0);
  system("pause");
  return 0;
}