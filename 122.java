//
// problem 122
//
//Answer 122 = 21035
//
//real	0m0.145s
//user	0m0.129s
//sys	0m0.032s
//
import java.util.*;
class problem122{
      public static void main(String arg[]){
// 
// LITTLE THINKING ON THAT PROBLEM SAYS THAT IF N (exponent) IS EVEN THEN THE RESIDUAL IS INDEPENDENT ON A AND IS IS ALWAYS 2
// OTHERWISE THE OPTIMAL N IS < A/2 AND THE RESIDUAL IS (A/2) * A * 2 
// SO THE TOTAL RESIDUAL IS MAX(2,(A/2) * A * 2)
//
//        int sum = 0 ; 
//	for (int i = 3 ; i <= 1000 ; i++) sum += Math.max(2,((i-1)/2)*i*2);

//      System.out.println("The result 120 is ..." + sum);

//
// NOW WE CAN EXTEND THAT TO THE PRIMES PROBLEM 123. IF THE P(N) IS THE EVEN PRIME, THE RESIDUAL IS 2. 
// FOR THE """""ODD""""" PRIMES THE RESIDUAL IS 2 * P_N * N > 10^10. 
//   

      long Current_Prime = 2 ;  
 
      for ( int i = 5 ; ; i+=2)
        if(IsPrimeWilson(i)){
        Current_Prime ++ ;
        if(2*Current_Prime*i>10000000000L&&Current_Prime%2==1) break;
      }
        System.out.println("Answer 122 = " + Current_Prime ) ;  
        
   }
 static boolean IsPrimeWilson(long input){
//
// easy tests 
// 
    if(input<=1L) return false;
    if(input==2L||input==3L) return true;
    if(input%2==0||input%3==0) return false;
//
//  lame looping   
//  note that we can start from 5 and jump over 6:
//  we try divisors i = 5 + j * 6 
//                  i = 5 + j * 6 + 1 ... 
//  we don't need to try +1 +3 +5 because they are even
//  and we don't need to try + 4 because 5 + j * 6 + 4 divides by 3
//
    int i = 5;
    while(i*i<=input) {
      if(input%i==0||input%(i+2)==0) return false   ;
       i += 6 ;
    } 
    return true;
 }

}


