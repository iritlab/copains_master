/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package chatbot;

import java.util.*; 

/**
 *
 * @author jorge
 */
public class Combinations {
    
// Java program to find combinations from n  
// arrays such that one element from each  
// array is present 

  
//class GFG{ 
  
// Function to print combinations that contain 
// one element from each of the given arrays 
static void print(Vector<String>[] arr) 
{ 
      
    // Number of arrays 
    int n = arr.length; 
  
    // To keep track of next element in  
    // each of the n arrays 
    int []indices = new int[n]; 
  
    // Initialize with first element's index 
    for(int i = 0; i < n; i++) 
        indices[i] = 0; 
  
    while (true) 
    { 
  
        // Print current combination 
        for(int i = 0; i < n; i++) 
            System.out.print(arr[i].get(indices[i]) + " "); 
                  
        System.out.println(); 
  
        // Find the rightmost array that has more 
        // elements left after the current element  
        // in that array 
        int next = n - 1; 
        while (next >= 0 &&  
              (indices[next] + 1 >=  
                   arr[next].size())) 
            next--; 
  
        // No such array is found so no more  
        // combinations left 
        if (next < 0) 
            return; 
  
        // If found move to next element in that  
        // array 
        indices[next]++; 
  
        // For all arrays to the right of this  
        // array current index again points to  
        // first element 
        for(int i = next + 1; i < n; i++) 
            indices[i] = 0; 
    } 
} 
  
// Driver code 
public static void main(String[] args) 
{ 
      
    // Initializing a vector with 3 empty vectors 
    @SuppressWarnings("unchecked") 
    Vector<String> []arr = new Vector[3]; 
    for(int i = 0; i < arr.length; i++) 
        arr[i] = new Vector<String>(); 
          
    // Now entering data 
    // [[1, 2, 3], [4], [5, 6]] 
    arr[0].add("land"); 
    arr[0].add("water"); 
    arr[1].add("indoor"); 
    arr[1].add("outdoor"); 
    arr[1].add("mixed"); 
    arr[2].add("single"); 
    arr[2].add("team"); 
    arr[2].add("mixed"); 
  
    print(arr); 
} 
} 
  
// This code is contributed by amal kumar choubey 
