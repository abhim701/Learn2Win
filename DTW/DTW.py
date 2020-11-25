
series1 = pd.Series([1,4,5,10,9,3,2,6,8,4])
series2 = pd.Series([1,7,3,4,1,10,5,4,7,4])

def fill_dtw_cost_matrix(s1, s2):
    l_s_1, l_s_2 = len(s1), len(s2)
    cost_matrix = np.zeros((l_s_1+1, l_s_2+1))
    for i in range(l_s_1+1):
        for j in range(l_s_2+1):
            cost_matrix[i, j] = np.inf
    cost_matrix[0, 0] = 0
    
    for i in range(1, l_s_1+1):
        for j in range(1, l_s_2+1):
            cost = abs(s1[i-1] - s2[j-1])
            #take last min from the window
            prev_min = np.min([cost_matrix[i-1, j], cost_matrix[i, j-1], cost_matrix[i-1, j-1]])
            cost_matrix[i, j] = cost + prev_min
    return cost_matrix
  
  
  dtw_cost_matrix = fill_dtw_cost_matrix(series1,series2)
