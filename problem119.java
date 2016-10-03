import java.util.*;
// Here we implement problem 119
//
// SOLUTION: FIND ALL THE NUMBERS BETWEEN 10^1 AND 10^N
//
//   THE POSSIBLE SUMS OF THE DIGITS ARE [2 .. N * 9 ] (1 IS ONLY FOR 10 100 1000 .. DOES NOT MATTER) 
//    
//   LOOP OVER GIVEN N 
//
//       LOOP FROM 2 -- N * 9 
//    
//        FOR EACH ENTRY CHECK THE INTEGERS YOU NEED TO CONSIDER AS POWERS TO BE BETWEEN 10 AND 10^N 
//        COMPUTE THE POWERS AND CHECK IF THE NUMBERS ARE OK. 
//        
//       END LOOP
//  
//   CHECK IF YOU HAVE REQUIRED NUMBER OF TERMS AND SORT THEM OUT
//
//   PRINT THE DATA 
// 
//
class problem119{
      public static void main(String arg[]){
      int des = 32 ; 
      ArrayList<Long> win = new ArrayList<Long>();
       

            for(int n=1; ; ){
//
// LOOP OVER THE POSSIBLE SUMS 
//
              for ( int possible_sum = 2 ; possible_sum <= n * 9 ; possible_sum++){
// 
// FOR GIVEN POSSIBLE SUM, CHECK WHAT ARE THE LEVELS YOU NEED TO CONSIDER BETWEEN 10^N 10^(N+1)
//
	      double low_bD = (double) n     / Math.log10( (double) possible_sum );
              double big_bD = (double) (n+1) / Math.log10( (double) possible_sum );
//
// BONDS ARE INCLUSIVE
//
              int    low_b  = (int) low_bD + 1 ; 
              int    big_b  = (int) big_bD     ;              
//
// LOOP FOR THAT SUM, CHECK IF IT WORKS, APPEND TO WIN
//
	         for(int try_it = low_b ; try_it <= big_b ; try_it ++){  
 
                     long test = pow(possible_sum,try_it);
                     if(SumDigits(test)==possible_sum) {win.add(test);} 
                 }
//
// FINISH THE LOOP YOU NEED TO MAKE SURE YOU TEST ALL THE NUMBERS UNDER 10^N AND 10^N+1 AS THEY ARE NOT ORDERED 
//
              }
//
// CHECK IF YOU HAVE ENOUGH
//
	if(win.size()>= des) break;
//
// ELSE MAKE A LARGER RANGE
//
	n++ ; 
        }

        SortList(win);
     }


     static long SumDigits(long input){
       long input_copy = input ;
       long sum = 0;
       while(input_copy > 0 ) {  
         long d , r ; 
         r = input_copy / 10 ; 
         d = input_copy - r * 10 ;
	 input_copy = r ; 
         sum += d ;
       }	  
         return sum ;    
    } 
     static long pow(int b , int e){
       long power = 1;
       for(int j = 1 ; j<= e; j++) power *= b ;
       return power;
    } 

     static void SortList(ArrayList<Long> input){
     int          size = input.size();
     long temp_array[] = new long[size];
     long t  ; 
     for(int i = 0 ; i < size ; i++) temp_array[i] = input.get(i);

       for(int a = 1     ; a<size  ;a++){
       for(int b = size-1; b>=a    ;b--){
         if( temp_array[b-1] > temp_array[b] ) {// make the switch
             t = temp_array[b-1];
             temp_array[b-1] = temp_array[b];
             temp_array[b]   = t;
         }
       }
       }
     for(int i = 0 ; i < size ; i++) input.add(temp_array[i]);
     for(int i = 0 ; i < size ; i++) System.out.println((i+1)+ " : "+ input.get(i));	 
     				     System.out.println("Answer = " + input.get(29));
	
     }
}	  


