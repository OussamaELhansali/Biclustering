#include <Rcpp.h>
using namespace Rcpp;

#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>

#include <string>



std::vector<int> row_seed(const std::vector<std::vector<int>>& mat) {
  std::vector<int> row_sums(mat.size());
  for (int i = 0; i < mat.size(); ++i) {
    row_sums[i] = std::accumulate(mat[i].begin(), mat[i].end(), 0);
  }
  int max_sum = *std::max_element(row_sums.begin(), row_sums.end());
  std::vector<int> result;
  for (int i = 0; i < row_sums.size(); ++i) {
    if (row_sums[i] == max_sum) {
      result.push_back(i);
    }
  }
  return result;
}

std::vector<int> column_seed(const std::vector<std::vector<int>>& mat) {
  std::vector<int> column_sums(mat[0].size());
  for (int j = 0; j < mat[0].size(); ++j) {
    for (int i = 0; i < mat.size(); ++i) {
      column_sums[j] += mat[i][j];
    }
  }
  int max_sum = *std::max_element(column_sums.begin(), column_sums.end());
  std::vector<int> result;
  for (int i = 0; i < column_sums.size(); ++i) {
    if (column_sums[i] == max_sum) {
      result.push_back(i);
    }
  }
  return result;
}


int row_diff(std::vector<int> row, std::vector<int> seed){
  int c = 0;
  for (int i = 0; i < seed.size(); i++){
    if (seed[i] != row[i]){
      c++;
    } 
  }
  return c;
}

std::vector<int> Row_difference(std::vector<std::vector<int>> E, std::vector<int> seed){
  std::vector<int> c(seed.size(), 0);
  for (int i = 0; i < E.size(); i++){
    c[i] = row_diff(E[i], seed);
  }
  return c;
}

std::vector<std::vector<int>> Row_cluster(const std::vector<std::vector<int>>& E, int row_threshold, int i) {
  auto seed = E[Row_seed(E)[i]];
  auto row_DV = Row_difference(E, seed);
  std::vector<int> list;
  for (int i = 0; i < row_DV.size(); i++) {
    if (row_DV[i] >= row_threshold) {
      list.push_back(i);
    }
  }
  if (list.empty()) {
    return E;
  }
  std::vector<std::vector<int>> result;
  for (int i = 0; i < E.size(); i++) {
    if (std::find(list.begin(), list.end(), i) == list.end()) {
      result.push_back(E[i]);
    }
  }
  return result;
}


std::vector<std::vector<double>> Col_cluster(std::vector<std::vector<double>> &E, double col_threshold, int i) {
  std::vector<double> seed;
  for (int j = 0; j < E[i].size(); j++) {
    seed.push_back(E[j][i]);
  }
  
  std::vector<double> col_DV;
  for (int j = 0; j < E.size(); j++) {
    double difference = 0;
    for (int k = 0; k < seed.size(); k++) {
      difference += abs(E[j][k] - seed[k]);
    }
    col_DV.push_back(difference);
  }
  
  std::vector<int> list;
  for (int j = 0; j < col_DV.size(); j++) {
    if (col_DV[j] > col_threshold) {
      list.push_back(j);
    }
  }
  
  std::vector<std::vector<double>> result;
  if (list.empty()) {
    return E;
  } else {
    for (int j = 0; j < E.size(); j++) {
      if (std::find(list.begin(), list.end(), j) == list.end()) {
        result.push_back(E[j]);
      }
    }
    return result;
  }
}

std::vector<std::vector<double>> max_sum_matrix(std::vector<std::vector<std::vector<double>>> &matrix_list) {
  double max_sum = -INFINITY;
  std::vector<std::vector<double>> max_matrix = matrix_list[0];
  
  for (int i = 0; i < matrix_list.size(); i++) {
    double sum = 0;
    for (int j = 0; j < matrix_list[i].size(); j++) {
      for (int k = 0; k < matrix_list[i][j].size(); k++) {
        sum += matrix_list[i][j][k];
      }
    }
    if (sum > max_sum) {
      max_sum = sum;
      max_matrix = matrix_list[i];
    }
  }
  
  return max_matrix;
}

vector<vector<int>> AMBB2(vector<vector<int>> &E, int row_threshold, int col_threshold) {
  vector<vector<int>> row_seeds = Row_seed(E);
  vector<vector<int>> Bicluster;
  for (int i = 0; i < row_seeds.size(); i++) {
    vector<vector<int>> row_matrix = Row_cluster(E, row_threshold, i);
    vector<vector<int>> col_seeds = Row_seed(row_matrix);
    for (int j = 0; j < col_seeds.size(); j++) {
      vector<vector<int>> col_matrix = Col_cluster(row_matrix, col_threshold, j);
      if (!col_matrix.empty()) {
        if (find(col_matrix.begin(), col_matrix.end(), 0) == col_matrix.end()) {
          Bicluster.push_back(col_matrix);
        } else {
          Bicluster.push_back(AMBB2(col_matrix, 1, 1));
        }
      }
    }
  }
  return max_sum_matrix(Bicluster);
}


using namespace std;

vector<vector<int>> permute_rows(vector<vector<int>> &M, int row1, int row2) {
  string rows[M.size()];
  for (int i = 0; i < M.size(); i++) {
    rows[i] = to_string(i);
  }
  swap(rows[row1], rows[row2]);
  swap(M[row1], M[row2]);
  return M;
}

vector<vector<int>> permute_columns(vector<vector<int>> &M, int col1, int col2) {
  for (int i = 0; i < M.size(); i++) {
    swap(M[i][col1], M[i][col2]);
  }
  return M;
}

vector<vector<int>> permute(vector<vector<int>> &M, map<int, string> Bicluster_rows, map<int, string> Bicluster_cols) {
  for (int i = 0; i < Bicluster_rows.size(); i++) {
    int row = stoi(Bicluster_rows[i]);
    M = permute_rows(M, i, row);
  }
  for (int j = 0; j < Bicluster_cols.size(); j++) {
    int col = stoi(Bicluster_cols[j]);
    M = permute_columns(M, j, col);
  }
  return M;
}