
	                      !   xrk4.for
	
PROGRAM xrk4
!C driver for routine rk4
INTEGER N
PARAMETER(N=4)
INTEGER i,j
REAL bessj,bessj0,bessj1
REAL h,x,y(N),dydx(N),yout(N)
EXTERNAL derivs
x=1.0
y(1)=bessj0(x)
y(2)=bessj1(x)
y(3)=bessj(2,x)
y(4)=bessj(3,x)
call derivs(x,y,dydx)
write(*,'(/1x,a,t19,a,t31,a,t43,a,t55,a)')&
& 'Bessel Function:','J0','J1','J3','J4'
do 11 i=1,5
h=0.2*i
call rk4(y,dydx,N,x,h,yout,derivs)
write(*,'(/1x,a,f6.2)') 'For a step size of:',h
write(*,'(1x,a10,4f12.6)') 'RK4:',(yout(j),j=1,4) 
	write(*,'(1x,a10,4f12.6)') 'Actual:',bessj0(x+h),&
& bessj1(x+h),bessj(2,x+h),bessj(3,x+h)
11 continue
END
!***************
 SUBROUTINE derivs(x,y,dydx)
REAL x,y(*),dydx(*)
dydx(1)=-y(2)
dydx(2)=y(1)-(1.0/x)*y(2)
dydx(3)=y(2)-(2.0/x)*y(3)
dydx(4)=y(3)-(3.0/x)*y(4)
return
END


!***************************************
!rk4.for
	
SUBROUTINE rk4(y,dydx,n,x,h,yout,derivs)
INTEGER n,NMAX
REAL h,x,dydx(n),y(n),yout(n)
EXTERNAL derivs
PARAMETER (NMAX=50)
INTEGER i
REAL h6,hh,xh,dym(NMAX),dyt(NMAX),yt(NMAX)
hh=h*0.5
h6=h/6.
xh=x+hh
do 11 i=1,n
yt(i)=y(i)+hh*dydx(i)
11 continue
call derivs(xh,yt,dyt)
do 12 i=1,n
yt(i)=y(i)+hh*dyt(i)
12 continue
call derivs(xh,yt,dym)
do 13 i=1,n
yt(i)=y(i)+h*dym(i)
dym(i)=dyt(i)+dym(i)
13 continue
call derivs(x+h,yt,dyt)
do 14 i=1,n
yout(i)=y(i)+h6*(dydx(i)+dyt(i)+2.*dym(i))
	14 continue
return
END


!***********************************
!bessj0.for
	
FUNCTION bessj0(x)
REAL bessj0,x
REAL ax,xx,z
DOUBLE PRECISION p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,r1,r2,r3,r4,r5,r6,&
&s1,s2,s3,s4,s5,s6,y
SAVE p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,r1,r2,r3,r4,r5,r6,s1,s2,s3,s4,&
&s5,s6
DATA p1,p2,p3,p4,p5/1.d0,-.1098628627d-2,.2734510407d-4,&
&-.2073370639d-5,.2093887211d-6/, q1,q2,q3,q4,q5/-.1562499995d-1,&
&.1430488765d-3,-.6911147651d-5,.7621095161d-6,-.934945152d-7/
DATA r1,r2,r3,r4,r5,r6/57568490574.d0,-13362590354.d0, &
&651619640.7d0,-11214424.18d0,77392.33017d0,-184.9052456d0/,s1,s2,&
&s3,s4,s5,s6/57568490411.d0,1029532985.d0,9494680.718d0,&
&59272.64853d0,267.8532712d0,1.d0/
if(abs(x).lt.8.)then
y=x**2
bessj0=(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6)))))/(s1+y*(s2+y*(s3+y*&
&(s4+y*(s5+y*s6)))))
	else
ax=abs(x)
z=8./ax
y=z**2
xx=ax-.785398164
bessj0=sqrt(.636619772/ax)*(cos(xx)*(p1+y*(p2+y*(p3+y*(p4+y*  &
&p5))))-z*sin(xx)*(q1+y*(q2+y*(q3+y*(q4+y*q5)))))
endif
return
END


 !*************************************************bessj1.for
	
FUNCTION bessj1(x)
REAL bessj1,x
REAL ax,xx,z
DOUBLE PRECISION p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,r1,r2,r3,r4,r5,r6,&
&s1,s2,s3,s4,s5,s6,y
SAVE p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,r1,r2,r3,r4,r5,r6,s1,s2,s3,s4,&
&s5,s6
DATA r1,r2,r3,r4,r5,r6/72362614232.d0,-7895059235.d0, &
&242396853.1d0,-2972611.439d0,15704.48260d0,-30.16036606d0/,s1,s2,&
&s3,s4,s5,s6/144725228442.d0,2300535178.d0,18583304.74d0, &
&99447.43394d0,376.9991397d0,1.d0/
DATA p1,p2,p3,p4,p5/1.d0,.183105d-2,-.3516396496d-4, &
&.2457520174d-5,-.240337019d-6/, q1,q2,q3,q4,q5/.04687499995d0,&
&-.2002690873d-3,.8449199096d-5,-.88228987d-6,.105787412d-6/
if(abs(x).lt.8.)then
y=x**2
bessj1=x*(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6)))))/(s1+y*(s2+y*(s3+	&
&y*(s4+y*(s5+y*s6)))))
	else
ax=abs(x)
z=8./ax
y=z**2
xx=ax-2.356194491
bessj1=sqrt(.636619772/ax)*(cos(xx)*(p1+y*(p2+y*(p3+y*(p4+y*&
&p5))))-z*sin(xx)*(q1+y*(q2+y*(q3+y*(q4+y*q5)))))*sign(1.,x)
endif
return
END


!********************************bessj.for
	
FUNCTION bessj(n,x)
INTEGER n,IACC
REAL bessj,x,BIGNO,BIGNI
PARAMETER (IACC=40,BIGNO=1.e10,BIGNI=1.e-10)
!CU USES bessj0,bessj1
INTEGER j,jsum,m
REAL ax,bj,bjm,bjp,sum,tox,bessj0,bessj1
if(n.lt.2)pause 'bad argument n in bessj'
ax=abs(x)
if(ax.eq.0.)then
bessj=0.
else if(ax.gt.float(n))then
tox=2./ax
bjm=bessj0(ax)
bj=bessj1(ax)
do 11 j=1,n-1
bjp=j*tox*bj-bjm
bjm=bj
bj=bjp
11 continue
bessj=bj
	else
tox=2./ax
m=2*((n+int(sqrt(float(IACC*n))))/2)
bessj=0.
jsum=0
sum=0.
bjp=0.
bj=1.
do 12 j=m,1,-1
bjm=j*tox*bj-bjp
bjp=bj
bj=bjm
if(abs(bj).gt.BIGNO)then
bj=bj*BIGNI
bjp=bjp*BIGNI
bessj=bessj*BIGNI
sum=sum*BIGNI
endif
if(jsum.ne.0)sum=sum+bj
jsum=1-jsum
if(j.eq.n)bessj=bjp
	12 continue
sum=2.*sum-bj
bessj=bessj/sum
endif
if(x.lt.0..and.mod(n,2).eq.1)bessj=-bessj
return
END







