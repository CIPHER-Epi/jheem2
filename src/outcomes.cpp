#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector populate_outcomes_array(NumericVector desired_times,
                                      CharacterVector char_desired_times,
                                      int n_per_time,
                                      List new_values,
                                      NumericVector new_times,
                                      NumericVector old_values,
                                      CharacterVector old_times,
                                      int prior_sim_index) //NB: prior_sim_index is indexed from 1 (R standard)
{
    int n_desired_times = desired_times.length();
    
    double first_new_time = R_PosInf;
    double last_new_time = R_PosInf;
    if (new_times.length()>0)
    {
        first_new_time = new_times[0];
        last_new_time = new_times[new_times.length()-1];
    }
  
    int n_old_times = old_times.length();
    int old_time_index = 0;
    double *old_src;
    if (prior_sim_index > 0)
    {
        old_src = old_values.begin() + n_per_time * n_old_times * (prior_sim_index-1); 
    }
    
    NumericVector rv(n_per_time * n_desired_times);
    for (int t=0; t<n_desired_times; t++)
    {
        double time = desired_times[t];
        const char* char_time = char_desired_times[t];
        
        if (time < first_new_time || time > last_new_time)// pull from old
        {
            while (old_times[old_time_index] != char_time)
                old_time_index++;
            
            for (int i=0; i<n_per_time; i++)
            {
                rv[i*n_desired_times + t] = old_src[i*n_old_times + old_time_index];
            }
        }
        else // pull from new
        {
            NumericVector new_src = new_values[char_time];
            for (int i=0; i<new_src.length(); i++)
            {
                rv[i*n_desired_times + t] = new_src[i];
            }
        }
    }
    
    return (rv);
}


// // [[Rcpp::export]]
// List calculate_rate_to_proportion_values(NumericVector desired_times,
//                                          List bindings,
//                                          NumericVector binding_times,
//                                          double cumulative_interval)
// {
//     List rv(desired_times.length());
//     
//     return (rv);
// }
