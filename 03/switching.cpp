//
//  main.cpp
//  csn
//
//  Created by kymry burwell on 11/1/18.
//  Copyright © 2018 kymry burwell. All rights reserved.
//

#include <iostream>
#include <vector>
#include <fstream>
#include <sstream>
#include <random>
#include <algorithm>
#include <string>

using namespace std;


void read_data(vector<vector<int>> &adj, vector<vector<int>> &edges, string filename){
    
    // Open input filstream
    ifstream input(filename);
    
    // Read in edges line by line (edge by edge)
    string line;
    int counter = 0;
    while (getline(input, line)){
        std::istringstream edge(line);
        int vertex1, vertex2;
        edge >> vertex1;
        edge >> vertex2;
        adj[vertex1].push_back(vertex2);
        edges[counter].push_back(vertex1);
        edges[counter].push_back(vertex2);
        ++counter;
    }
    input.close();
}


void output_data(vector<vector<int>> &adj, string filename, int i){
    
    // Open output filestream connection
    ofstream output(filename + "_out" + to_string(i) + ".txt");
    
    // Output data to file
    for (auto beg = adj.begin(); beg != adj.end(); ++beg) // For each vertex in adj
        for (const auto &vertex : *beg){ // For each edge
            output << (beg - adj.begin()) << "," << vertex << endl;
        }
    output.close();
}

bool are_adjacent(vector<vector<int>> &adj, int v1, int v2){
    
    bool adjacent = false;
    for (auto &i : adj[v1])
            if (i == v2) adjacent = true;
    return adjacent;
    
}

void remove_edges(vector<vector<int>> &adj, int v1, int v2){
    
    vector<int> &vertex = adj[v1];
    vertex.erase(remove(vertex.begin(), vertex.end(), v2), vertex.end());

}

void switching(vector<vector<int>> &adj, vector<vector<int>> &edges, int E){
    
    // Data structures
    vector<int> u_v_id;
    vector<int> s_t_id;
    int QE = E*4;
    
    // Configure random number generator
    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<> dis(1, E); // SET RANGE HERE - (lower bound, upperbounds)
    
    // Generate random samples
    for (int i = 0; i < E*4; i++){
        u_v_id.push_back(dis(gen));
        s_t_id.push_back(dis(gen));
    }
    
    // Perform switchings
    for (int i = 0; i < QE; ++i){
        // Extract edges to be switched
        vector<int> u_v = edges[u_v_id[i]];
        vector<int> s_t = edges[s_t_id[i]];
        
        // Saved original edges
        vector<int> u_v_original = u_v;
        vector<int> s_t_original = s_t;
        
        // If safe, perform switches
        if (u_v[0] != s_t[0] && u_v[1] != s_t[1] && u_v[0] != s_t[1] && s_t[0] != u_v[1] &&
            !are_adjacent(adj,s_t[0], u_v[1]) && !are_adjacent(adj,u_v[1], s_t[0]) &&
            !are_adjacent(adj,u_v[0], s_t[1]) && !are_adjacent(adj,s_t[1], u_v[0])){
        
            int temp = u_v[1];
            u_v[1] = s_t[1];
            s_t[1] = temp;
            
            // Reassign edges in edgelist
            edges[u_v_id[i]] = u_v;
            edges[s_t_id[i]] = s_t;
            
            // Remove edges from adjacency list
            remove_edges(adj, u_v_original[0], u_v_original[1]);
            remove_edges(adj, s_t_original[0], s_t_original[1]);
            
            // Add new edges to adjacency list
            adj[u_v[0]].push_back(u_v[1]);
            adj[s_t[0]].push_back(s_t[1]);
        }
    }
}


int main(int argc, const char * argv[]) {
    
    // Number of times to perform switching
    constexpr int NUM_ITERS = 20;
    
    // Set input and output filename
    string infilename = "/Users/kymryburwell/Google Drive/UPC/Fall 2018/CSN/Labs/git_labs/Complex-and-Social-Networks/03/Czech.txt";
    string outfilename = infilename.substr(0,infilename.length()-4);

    for (int i = 0; i < NUM_ITERS; ++i){
        
        // Data structures: edge list and adjacency list
        constexpr int N = 69325, E = 257250;
        vector<vector<int>> adj(N);
        vector<vector<int>> edges(E);
        
        // Read in data
        read_data(adj, edges, infilename);
        
        // Perform switching
        switching(adj, edges, E);

        // Output data to file
        output_data(adj, outfilename, i);
    }
    return 0;
}













