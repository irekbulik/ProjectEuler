/*
 * =====================================================================================
 *
 *       Filename:  098.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/07/2018 21:36:28
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================
 */

#include <iostream>
#include <fstream>
#include <string> 
#include <vector>
#include <unordered_map> 
#include <algorithm>
#include <cmath>
using namespace std;

vector<string> get_words(string line){
    string word;
    bool   in_word = false;
    vector<string> ans;
    for(auto i=0;i<line.size();i++){
	     if(line[i]=='"'&&in_word)
		      ans.push_back(word),word.clear(),in_word=false;
	     else if(in_word)
		      word.push_back(line[i]);
	     else if(line[i]=='"'&&!in_word)
		      in_word=true;
    }
    return ans;
}

bool is_ps(long long i){
     long long j=sqrt(i);
     return i==j*j||i==(j+1)*(j+1);     
}


long long check_it(string word1, string word2){
     unordered_map<char,int> letters;
     unordered_map<int,bool> used;
     vector<vector<int>>     prev_map,map;
     long long max_val=0;
     int il=0;
     for(auto i=0;i<word1.size();i++)
	 if(letters.count(word1[i])==0)
	 	letters[word1[i]]=il++;
     vector<char> chars(letters.size());
     for(auto i=0;i<10;i++)
	  map.push_back({i});    

     for(auto id=1;id<letters.size();id++){
	 prev_map=map;
	 map.clear();
	 for(auto iv : prev_map){
           used.clear();		  
	    for(auto in : iv)
	       used[in]=true;	
           for(auto in=0;in<10;in++)
	 	if(used.count(in)==0)
	         map.push_back(iv),map.back().push_back(in); 	
	 }	  
     } 
     il=word1.size(); 	
     for(auto im : map){
        long long base=1;
	 long long number=0,number2;
	 for(auto i=0;i<im.size();i++){
	    number+=base*im[letters[word1[il-i-1]]],base*=10;	 
	 }
	 if(number<base/10) 
	    continue;		  
        if(is_ps(number)){
	    number2=0;
	    base=1;
	    for(auto i=0;i<im.size();i++){
	       number2+=base*im[letters[word2[il-i-1]]],base*=10;	
	    } 
	    if(is_ps(number2)&&number2>base/10){
	       max_val = max(max_val,max(number,number2));
	    }
	 }	  
     }
     return max_val;	
}


int main(){
    string line;
    ifstream input ("p098_words.txt");
    if(input.is_open())
	getline(input,line),input.close();
    vector<string> words=get_words(line);
    unordered_map<string,vector<string>> anagrams;
    long long max_val;

    for(auto i=0;i<words.size();i++){
	string key=words[i];
       sort(key.begin(),key.end());
	anagrams[key].push_back(words[i]);	
    }
// there are no more than 2 pairs 
    for(auto i : anagrams ){
	     if(i.second.size()==1) 
		      continue;
	     max_val = max(max_val,check_it(i.second[0],i.second[1]));
    }

    cout << "Answer 098 = " <<  max_val << endl;

    return 0;
}




