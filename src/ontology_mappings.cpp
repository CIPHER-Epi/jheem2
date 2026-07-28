// CODE TO SPEED UP ONTOLOGY //
    
    
    #include <Rcpp.h>
    #include <vector>
    using namespace Rcpp;


RObject do_execute_ontology_mapping(List src_dim_names,
                                    List dst_dim_names,
                                    NumericVector src,
                                    NumericVector dst,
                                    CharacterVector from_values,
                                    CharacterVector to_values,
                                    bool do_apply,
                                    bool get_matrix,
                                    bool na_rm)
{
    bool get_indices = !do_apply && !get_matrix;
    
    //----------------------------//
        //----------------------------//
        //-- PROCESS THE ATTRIBUTES --//
        //----------------------------//
        //----------------------------//
        
        //-- Process dst attributes --//
        if (dst_dim_names.attr("names")==R_NilValue)
            return (R_NilValue);
    CharacterVector dst_dimensions = dst_dim_names.names();
    
    int n_dst_dims = dst_dimensions.length();
    
    std::vector<int> dst_dims(n_dst_dims);
    CharacterVector tmp;
    for (int j=0; j<n_dst_dims; j++)
        dst_dims[j] = ((CharacterVector) dst_dim_names[j]).length();
    
    //-- Process src attributes --//
        if (src_dim_names.attr("names")==R_NilValue)
            return (R_NilValue);
    CharacterVector src_dimensions = src_dim_names.names();
    
    int n_src_dims = src_dimensions.length();
    
    std::vector<int> src_dims(n_src_dims);
    for (int j=0; j<n_src_dims; j++)
        src_dims[j] = ((CharacterVector) src_dim_names[j]).length();
    
    
    //-- Process from_values --//
        
        if (from_values.attr("dimnames")==R_NilValue)
            return (R_NilValue);
    List from_dim_names = from_values.attr("dimnames");
    if (from_dim_names.length()<2 || from_dim_names[1]==R_NilValue)
        return (R_NilValue);
    
    CharacterVector from_dimensions = from_dim_names[1];
    
    
    int n_from_dims = from_dimensions.length();
    int n_non_from_dims = n_src_dims - n_from_dims;
    
    std::vector<int> from_dim_indices(n_from_dims);
    std::vector<int> non_from_dim_indices(n_non_from_dims);
    
    std::vector<int> non_from_dims(n_non_from_dims);
    int n_non_from = 1;
    std::vector<int> n_before_non_from(n_non_from_dims);
    std::vector<int> n_before_from(n_from_dims);
    
    int non_from_dim_index = 0;
    int n_before = 1;
    
    // Figure out which of the src dimensions are ones that are being transformed by the mapping
    bool found_match;
    for (int i=0; i<n_src_dims; i++)
    {
        found_match = false;
        for (int j=0; j<n_from_dims && !found_match; j++)
        {
            if (src_dimensions[i] == from_dimensions[j])
            {
                found_match = true;
                
                from_dim_indices[j] = i;
                n_before_from[j] = n_before;
            }
        }
        
        if (!found_match)
        {
            non_from_dim_indices[non_from_dim_index] = i;
            non_from_dims[non_from_dim_index] = src_dims[i];
            n_non_from *= non_from_dims[non_from_dim_index];
            n_before_non_from[non_from_dim_index] = n_before;
            
            non_from_dim_index++;
        }
        
        n_before *= src_dims[i];
    }
    
    if (from_values.attr("dim")==R_NilValue)
        return (R_NilValue);
    IntegerVector from_dims = from_values.attr("dim");
    int n_values = from_dims[0];
    
    //-- Process to_values --//
        
        if (to_values.attr("dimnames")==R_NilValue)
            return (R_NilValue);
    List to_dim_names = to_values.attr("dimnames");
    if (to_dim_names.length()<2 || to_dim_names[1]==R_NilValue)
        return (R_NilValue);
    
    CharacterVector to_dimensions = (CharacterVector) to_dim_names[1];
    std::vector<int> which_to_dimensions_to_use(to_dimensions.length());
    
    // Calculate the number of 'to' dimensions actually in the array
    //  We are allowed to ignore omitted to dimensions (as long as at least one is included)
    int n_to_dims = 0;
    for (int j=0; j<to_dimensions.length(); j++)
    {
        for (int k=0; k<dst_dimensions.length(); k++)
        {
            if (to_dimensions[j] == dst_dimensions[k])
            {
                which_to_dimensions_to_use[n_to_dims] = j;
                n_to_dims++;
                break;
            }
        }
    }
    
    if (n_to_dims==0)
        return (R_NilValue);
    
    int n_non_to_dims = n_dst_dims - n_to_dims;
    
    std::vector<int> to_dim_indices(n_to_dims);
    std::vector<int> non_to_dim_indices(n_non_to_dims+1);
    
    std::vector<int> non_to_dims(n_non_to_dims);
    std::vector<int> n_before_non_to(n_non_to_dims);
    std::vector<int> n_before_to(n_to_dims);
    
    int non_to_dim_index = 0;
    n_before = 1;
    
    for (int i=0; i<n_dst_dims; i++)
    {
        found_match = false;
        for (int j=0; j<n_to_dims && !found_match; j++)
        {
            if (dst_dimensions[i] == to_dimensions[which_to_dimensions_to_use[j] ])
            {
                found_match = true;
                to_dim_indices[j] = i;
                n_before_to[j] = n_before;
            }
        }
        
        if (!found_match) 
        {
            non_to_dim_indices[non_to_dim_index] = i;
            non_to_dims[non_to_dim_index] = dst_dims[i];
            n_before_non_to[non_to_dim_index] = n_before;
            
            non_to_dim_index++;
        }
        
        n_before *= dst_dims[i];
    }
    
    if (to_values.attr("dim")==R_NilValue)
        return (R_NilValue);
    IntegerVector to_dims = to_values.attr("dim");
    if (to_dims[0] != n_values)
        return (R_NilValue);
    
    //----------------------------------//
        //----------------------------------//
        //-- SET UP THE DIMENSION INDICES --//
        //----------------------------------//
        //----------------------------------//
        
        int n_dst = 1;
    for (int i=0; i<n_dst_dims; i++)
        n_dst *= dst_dims[i];
    
    // Map from the 'from_values' to the src dimension values
    //  (ie parse the integer value for each from_dim_values)
    std::vector<std::vector<int>> from_dim_values(n_values, std::vector<int>(n_from_dims));
    int n_mapped_from = 0;
    CharacterVector dim_values;
    std::vector<int> mapped_from_values_indices(n_values);
    
    bool all_dims_mapped;
    for (int i=0; i<n_values; i++)
    {
        all_dims_mapped = true;
        for (int j=0; j<n_from_dims && all_dims_mapped; j++)
        {
            dim_values = src_dim_names[from_dim_indices[j]];
            from_dim_values[i][j] = -1;
            
            for (int k=0; k<dim_values.length(); k++)
            {
                if (from_values[j*n_values + i] == dim_values[k])
                {
                    from_dim_values[n_mapped_from][j] = k;
                    break;
                }
            }
            
            // if we didn't find a match, we will just ignore this row in from_values/to_values
            if (from_dim_values[n_mapped_from][j]==-1)
                all_dims_mapped = false;
        }
        
        if (all_dims_mapped)
        {
            mapped_from_values_indices[n_mapped_from] = i;
            n_mapped_from++;
        }
    }
    
    
    std::vector<std::vector<int>> to_dim_values(n_mapped_from, std::vector<int>(n_to_dims));
    int n_mapped = 0;
    std::vector<int> mapped_to_values_indices(n_mapped_from);
    int j_in_values;
    
    for (int i=0; i<n_mapped_from; i++)
    {
        all_dims_mapped = true;
        for (int j=0; j<n_to_dims && all_dims_mapped; j++)
        {
            j_in_values = which_to_dimensions_to_use[j];
            dim_values = dst_dim_names[to_dim_indices[j]];
            to_dim_values[i][j] = -1;
            for (int k=0; k<dim_values.length(); k++)
            {
                if (to_values[ j_in_values*n_values + mapped_from_values_indices[i] ] == dim_values[k])
                {
                    to_dim_values[n_mapped][j] = k;
                    break;
                }
            }
            
            if (to_dim_values[n_mapped][j]==-1)
                all_dims_mapped = false;
        }
        
        // if we didn't find a match, we will just ignore this row in from_values/to_values
            if (all_dims_mapped)
            {
                mapped_to_values_indices[n_mapped] = i;
                n_mapped++;
            }
        }
        
        //   if (n_mapped==0) //there is nothing to map - no changes to make to dst
        //        return (dst);
        
        
        // cut out any from values that were not used
        for (int i=0; i<n_mapped; i++)
        {
            for (int j=0; j<n_from_dims; j++)
                from_dim_values[i][j] = from_dim_values[ mapped_to_values_indices[i] ][j];
        }
        
        int max_non_from_dim=0;
        for (int j=0; j<n_non_from_dims; j++)
        {
            if (non_from_dims[j] > max_non_from_dim)
                max_non_from_dim = non_from_dims[j];
        }
        
        
        // Map non_from to non_to dimensions
        std::vector<int> non_to_to_non_from_dims(n_non_to_dims);
        
        for (int j=0; j<n_non_to_dims; j++)
        {
            non_to_to_non_from_dims[j] = -1;
            for (int k=0; k<n_non_from_dims; k++)
            {
                if (dst_dimensions[ non_to_dim_indices[j] ] == src_dimensions[ non_from_dim_indices[k] ])
                {
                    non_to_to_non_from_dims[j] = k;
                    break;
                }
            }
            
            if (non_to_to_non_from_dims[j]==-1)
                return (R_NilValue);
        }
        
        // Map non_from to non_to dimension values
        std::vector<std::vector<int>> non_from_to_non_to_dim_values(n_non_to_dims, std::vector<int>(max_non_from_dim));
        std::vector<std::vector<int>> included_non_from_dim_values(n_non_from_dims, std::vector<int>(max_non_from_dim));
        std::vector<int> n_included_for_non_from_dim(n_non_from_dims);
        for (int j=0; j<n_non_from_dims; j++)
        {
            n_included_for_non_from_dim[j] = non_from_dims[j];
            for (int k=0; k<non_from_dims[j]; k++)
            {
                included_non_from_dim_values[j][k] = k;
            }
        }
        // Note: a source dimension value is "included" if it is present in dst - otherwise it is ignored
        
        CharacterVector src_values;
        CharacterVector dst_values;
        for (int i=0; i<n_non_to_dims; i++)
        {
            int non_from_d = non_to_to_non_from_dims[i];
            src_values = (CharacterVector) src_dim_names[non_from_dim_indices[non_from_d]];
            dst_values = (CharacterVector) dst_dim_names[non_to_dim_indices[i]];
            
            n_included_for_non_from_dim[non_from_d] = 0;
            
            for (int j=0; j<src_values.length(); j++)
            {
                non_from_to_non_to_dim_values[i][j] = -1;
                for (int k=0; k<dst_values.length(); k++)
                {
                    if (src_values[j] == dst_values[k])
                    {
                        non_from_to_non_to_dim_values[i][j] = k;
                        break;
                    }
                }
                
                if (non_from_to_non_to_dim_values[i][j] != -1)
                {
                    included_non_from_dim_values[non_from_d][ n_included_for_non_from_dim[non_from_d] ] = j;
                    n_included_for_non_from_dim[non_from_d]++;
                }
            }
            
            if (n_included_for_non_from_dim[non_from_d]==0)
                return(R_NilValue); //we don't have any src dimension values present in dst for the dimension
    }
    
    int n_included_non_from = 1;
    for (int j=0; j<n_non_from_dims; j++)
        n_included_non_from *= n_included_for_non_from_dim[j];
    
    //----------------------------------------------//
    //----------------------------------------------//
    //-- ITERATE THROUGH AND MOVE from SRC to DST --//
    //----------------------------------------------//
    //----------------------------------------------//
    
    // temp variable only if we are applying
    int n_for_untouched = (do_apply) * n_dst;
    std::vector<bool> dst_is_untouched(n_for_untouched);
    for (int i=0; i<n_for_untouched; i++)
        dst_is_untouched[i] = true;
    
    // temp variables only if we are getting indices
    
    std::vector<int> n_mapped_to_dst((get_indices) * n_dst);
    if (get_indices)
    {
        for (int i=0; i<n_dst; i++)
            n_mapped_to_dst[i] = 0;
    }
    
    
    int n_for_indices = n_included_non_from * n_mapped;
    std::vector<int> dst_index_for_indices((get_indices) * n_for_indices);
    std::vector<int> src_index_for_indices((get_indices) * n_for_indices);
    int index_for_indices = 0;
    
    // temp variables for the iteration
    std::vector<int> non_from_dim_value_indices(n_non_from_dims);
    for (int j=0; j<n_non_from_dims; j++)
        non_from_dim_value_indices[j] = 0;
    
    std::vector<int> non_from_dim_values(n_non_from_dims);
    for (int j=0; j<n_non_from_dims; j++)
        non_from_dim_values[j] = included_non_from_dim_values[j][0];
    
    int base_src_index, src_index, base_dst_index, dst_index;
    
    
    // iterate
    for (int i=0; i<n_included_non_from; i++)
    {
        base_src_index = 0;
        for (int j=0; j<n_non_from_dims; j++)
            base_src_index += n_before_non_from[j] * non_from_dim_values[j];
        
        base_dst_index = 0;
        for (int j=0; j<n_non_to_dims; j++)
            base_dst_index += n_before_non_to[j] * 
                non_from_to_non_to_dim_values[j][ non_from_dim_values[non_to_to_non_from_dims[j]] ];;
        
        for (int map_index=0; map_index<n_mapped; map_index++)
        {
            src_index = base_src_index;
            for (int j=0; j<n_from_dims; j++)
                src_index += n_before_from[j] * from_dim_values[map_index][j];
            
            dst_index = base_dst_index;
            for (int j=0; j<n_to_dims; j++)
                dst_index += n_before_to[j] * to_dim_values[map_index][j];
            
            if (do_apply)
            {
                if (!na_rm || !NumericVector::is_na(src[src_index])) // write the value
                {
                    if (dst_is_untouched[dst_index])
                    {
                        dst[dst_index] = src[src_index];
                        dst_is_untouched[dst_index] = false;
                    }
                    else
                        dst[dst_index] += src[src_index];
                }
            }
            else if (get_matrix)
            {
                dst[dst_index + n_dst*src_index] = 1;
            }
            else // get_indices
            {
                n_mapped_to_dst[dst_index]++;
                dst_index_for_indices[index_for_indices] = dst_index;
                src_index_for_indices[index_for_indices] = src_index;
                index_for_indices++;
            }
        }
        
        // Update the non_from_dim_values
        for (int j=0; j<n_non_from_dims; j++)
        {
            if (non_from_dim_value_indices[j]==(n_included_for_non_from_dim[j]-1))
            {
                non_from_dim_value_indices[j] = 0;
                non_from_dim_values[j] = included_non_from_dim_values[j][0];
            }
            else
            {
                non_from_dim_value_indices[j]++;
                non_from_dim_values[j] = included_non_from_dim_values[j][ non_from_dim_value_indices[j] ];
                break;
            }
        }
    }
    
    NumericVector dst_vector;
    if (get_indices)
    {
        List rv(n_dst);
        
        // Allocate the numeric vectors for the list rv
        NumericVector empty_vector(0);
        for (int i=0; i<n_dst; i++)
        {
            if (n_mapped_to_dst[i]==0)
                rv[i] = empty_vector;
            else
                rv[i] = NumericVector(n_mapped_to_dst[i]);
            
            n_mapped_to_dst[i] = 0;
        }
        
        // Populate the numeric vectors in the list rv
        for (int j=0; j<n_for_indices; j++)
        {
            dst_index = dst_index_for_indices[j];
            dst_vector = rv[dst_index];
            dst_vector[ n_mapped_to_dst[dst_index] ] = src_index_for_indices[j]+1;
            n_mapped_to_dst[dst_index]++;
        }
        
        return (rv);
    }
    else    //-- Return --//
        return (dst);
}


// [[Rcpp::export]]
RObject apply_ontology_mapping(NumericVector src,
                               NumericVector dst,
                               CharacterVector from_values,
                               CharacterVector to_values,
                               bool na_rm)
{   
    //-- Process dst attributes --//
    if (dst.attr("dimnames")==R_NilValue)
        return (R_NilValue);
    List dst_dim_names = dst.attr("dimnames");
    
    //-- Process src attributes --//
    if (src.attr("dimnames")==R_NilValue)
        return (R_NilValue);
    List src_dim_names = src.attr("dimnames");
    
    //-- Call the sub-function --//
    return (do_execute_ontology_mapping(src_dim_names,
                                        dst_dim_names,
                                        src,
                                        dst,
                                        from_values,
                                        to_values,
                                        true,
                                        false,
                                        na_rm));
}

NumericVector EMPTY_PLACEHOLDER_VECTOR(0);

// [[Rcpp::export]]
RObject get_ontology_mapping_matrix(List src_dim_names,
                                    List dst_dim_names,
                                    NumericVector dst,
                                    CharacterVector from_values,
                                    CharacterVector to_values)
{
    return (do_execute_ontology_mapping(src_dim_names,
                                        dst_dim_names,
                                        EMPTY_PLACEHOLDER_VECTOR,
                                        dst,
                                        from_values,
                                        to_values,
                                        false,
                                        true,
                                        true));
}

// [[Rcpp::export]]
RObject get_ontology_mapping_indices(List src_dim_names,
                                     List dst_dim_names,
                                     CharacterVector from_values,
                                     CharacterVector to_values)
{
    return (do_execute_ontology_mapping(src_dim_names,
                                        dst_dim_names,
                                        EMPTY_PLACEHOLDER_VECTOR,
                                        EMPTY_PLACEHOLDER_VECTOR,
                                        from_values,
                                        to_values,
                                        false,
                                        false,
                                        true));
}

// [[Rcpp::export]]
IntegerVector do_get_reverse_indices_from_forward(List forward_indices,
                                                  int n_from)
{
    int n_forward = forward_indices.length();
    
    IntegerVector rv(n_from);
    for (int to_index=0; to_index<n_from; to_index++)
    {
        bool still_missing = true;
        for (int i=0; still_missing && i<n_forward; i++)
        {
            IntegerVector forward_i = forward_indices[i];
            for (int j=0; j<forward_i.length(); j++)
            {
                if (forward_i[j] == (to_index+1))
                {
                    rv[to_index] = i+1;
                    still_missing = false;
                    break;
                }
            }
        }
    }
    
    return (rv);
}