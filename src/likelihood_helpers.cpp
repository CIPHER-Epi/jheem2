#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector generate_transformation_matrix_indices(
        NumericVector transformation_matrix,
        int m,
        int n) {
    
    // Create sparse representation of the transformation matrix 
    int sparse_indices[n*m];
    int sparse_column_length[n];
    int *sparse_column;
    
    for (int i=0; i<n; i++) {
        sparse_column = sparse_indices + i*m;
        sparse_column_length[i] = 0;
        for (int j=0; j<m; j++) {
            if (transformation_matrix[i*m + j]) {
                sparse_column[ sparse_column_length[i] ] = j;
                sparse_column_length[i]++;
            }
        }
    }
    
    // Figure out how many entries we have in sparse_indices
    int n_triples = 0;
    for (int i=0; i<n; i++)
    {
        for (int j=0; j<sparse_column_length[i]; j++)
        {
            n_triples += sparse_column_length[i] - j;
        }
    }
    
    // Convert to a vector consisting of triples (i,j,k)
    // Note that for space reasons, (2,3,k) will be present but not (3,2,k)
    IntegerVector triples(n_triples * 3);
    int triples_index = 0;
    for (int k=0; k<n; k++) {
        sparse_column = sparse_indices + k*m;
        for (unsigned int a=0; a<sparse_column_length[k]; a++) {
            for (unsigned int b=a; b<sparse_column_length[k]; b++) {
                triples[triples_index] = sparse_column[a];
                triples[triples_index + 1] = sparse_column[b];
                triples[triples_index + 2] = k;
                
                triples_index += 3;
            }
        }
    }
    
    // Return the IntegerVector triples
    return triples;
}

// [[Rcpp::export]]
IntegerVector OLD_generate_transformation_matrix_indices(
        NumericVector transformation_matrix,
        int m,
        int n) {
    
    // Create sparse representation of the transformation matrix 
    List sparse_indices (n);
    for (int i=0; i<n; i++) {
        IntegerVector sparse_column;
        for (int j=0; j<m; j++) {
            if (transformation_matrix[i*m + j]) {
                sparse_column.push_back(j);
            }
        }
        sparse_indices[i] = sparse_column;
    }
    
    // Convert to a vector consisting of triples (i,j,k)
    // Note that for space reasons, (2,3,k) will be present but not (3,2,k)
    IntegerVector triples;
    for (int k=0; k<n; k++) {
        IntegerVector sparse_column = sparse_indices[k];
        for (unsigned int a=0; a<sparse_column.length(); a++) {
            for (unsigned int b=a; b<sparse_column.length(); b++) {
                triples.push_back(sparse_column[a]);
                triples.push_back(sparse_column[b]);
                triples.push_back(k);
            }
        }
    }
    
    // Return the IntegerVector triples
    return triples;
}

// [[Rcpp::export]]
RObject generate_transformation_matrix_row_oriented_indices(
        NumericVector transformation_matrix,
        int m,
        int n) {
    
    // Create sparse representation of the transformation matrix
    List sparse_indices (m);
    for (int i=0; i<m; i++) {
        IntegerVector sparse_row;
        for (int j=0; j<n; j++) {
            if (transformation_matrix[j*m + i]) {
                sparse_row.push_back(j);
            }
        }
        sparse_indices[i] = sparse_row;
    }
    
    // Return the List
    return sparse_indices;
    
}


// [[Rcpp::export]]
NumericVector get_basic_likelihood_mean(
        const NumericVector sim_numerator,
        const List transformation_matrix_row_oriented_indices,
        int m,
        NumericVector mean
) {
    for (int i=0; i<m; i++) {
        IntegerVector sparse_row = transformation_matrix_row_oriented_indices[i];
        for (int j=0; j<sparse_row.length(); j++) {
            mean[i] += sim_numerator[sparse_row[j]];
        }
    }
    return mean;
}


/*
 * This function computes the transformed covariance matrix using using only the
 * nonzero operations in A = MVM^T. First, the sparse, binary transformation
 * matrix was stored in a condensed form recording which rows have nonzero
 * elements in each column. Each position i,j in the transformed covariance
 * matrix is the sum of the products along one row of MV and one column of M^T,
 * which is itself a row of M. In the dense case, this will be the sum of *m*
 * products. In the actual sparse case, only a few of these products will be
 * nonzero and contribute to the sum, and this occurs exactly when there is a
 * nonzero element in the same column for a pair of rows in M (one of which
 * becomes a column of M^T). If a column *k* of M has nonzero elements in *s*
 * different rows, then that column will contribute a product to the positions
 * i,j in A where i,j is every pairing of the rows *s*. Other columns may
 * contribute to that position i,j, but if we loop across columns of M and
 * merely add the contribution from each column as it comes, we will eventually
 * create the full sum of nonzero products comprising A[i,j]. The role of the
 * covariance matrix itself is simple since it is diagonal; each column *k* of M
 * will be multiplied by the kth diagonal element of the covariance matrix.
 * Therefore, each contribution to A[i,j] becomes 1 * V[k,k] = V[k,k]. Creating
 * the sparse representation occurs during instantiate time. I believe the
 * transformation algorithm here has a big-O of O(k*s^2) where *k* is the number
 * of columns with nonzero elements and *s* is the number of rows with nonzero
 * elements in each column. While *k* is expected to be the same as the width of
 * M, *n*, *s* is expected to be quite small. In the limit as *k* goes to *n*
 * and *s* goes to *m*, this algorithm approaches regular matrix multiplication
 * runtime. The reason *s* is squared is because this is number of pairs of s
 * elements. Making sigma will take *m*-squared time, possibly making it the
 * more expensive operation.
 */

// [[Rcpp::export]]
NumericVector get_basic_likelihood_sigma(
        const NumericVector sim_numerator,
        const NumericVector sim_denominator,
        const NumericVector transformation_matrix_indices,
        const NumericVector measurement_error_cov_matrix,
        int m,
        NumericVector sigma,
        const bool Poisson
) {
    // generate model imperfection covariance diagonal matrix
    /*
     double* V;
     double binomial_variance[sim_numerator.length() * !Poisson]; // no allocation of memory because it's on the stack, so no more expensive regardless of saying * !Poisson
     
     if (Poisson) {
     V = *sim_numerator;
     } else {
     for (int i=0; i<sim_numerator.length(); i++) {
     binomial_variance[i] = sim_numerator[i] * (1 - sim_numerator[i] / sim_denominator[i]);
     }
     V = binomial_variance;
     }*/
    
    
    // copy measurement_error_cov_matrix into sigma
    for (int i=0; i<measurement_error_cov_matrix.length(); i++) {
        sigma[i] = measurement_error_cov_matrix[i];
    }
    
    // add to sigma MVM^T
    int previous_k = -1;
    double value_to_add;
    for (int h=0; h<transformation_matrix_indices.length(); h+=3) {
        int i = transformation_matrix_indices[h];
        int j = transformation_matrix_indices[h + 1];
        int k = transformation_matrix_indices[h + 2];
        
        if (k != previous_k){
            previous_k = k;
            if (Poisson)
                value_to_add = sim_numerator[k];
            else if (sim_denominator[k] == 0)
                value_to_add = 0;  // no denominator (incl. tiny ones zeroed in R) -> no binomial variance
            else
                value_to_add = sim_numerator[k] * (1 - sim_numerator[k] / sim_denominator[k]);
        }
        
        sigma[j*m + i] += value_to_add;
        
        if (i != j) {
            sigma[i*m + j] += value_to_add;
        }
    }
    
    return sigma;
}