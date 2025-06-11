#Python version for the real-time slope based procedure for abrupt shift detection:
import numpy as np 
import pandas as pd
from statsmodel import OLS
from collections import defaultdict
num_list = []
with open('readin.txt', 'r') as file:
    for line in file:
        num_list.append(line.split(" "))

def slope_detection(ts, lowwl = 5, highwl = 'default'):
	l = len(ts)
	if highwl == 'default':
		highwl = l // 3
	wls = range(lowwl, highwl + 1)
	asdetect_score = np.zeros(l)
	outlier_ind_up_map = defaultdict(list)
	outlier_ind_down_map = defaultdict(list)
	slope_dict = defaultdict(list)
	for window_length in wls:
		breaks = l / window_length
		#slight modification for sake of online update:
	    if breaks != int(breaks):
	    	breaks = int(np.floor(breaks))
	    	remainder = l - breaks * window_length
	    	start = int(np.floor(remainder/2))
	    	ts_temp = ts[start:start+breaks*window_length]
	    else:
	    	breaks = int(breaks)
	    	remainder = None
	    	ts_temp = None
        lm_coeffs = np.ones((breaks, 2))
        for i in range(breaks):
            X = np.arange(1, window_length + 1)
            Y = ts_temp[i * window_length, (i+1)*window_length]
            X = add_constant(X)
            model = OLS(Y, X).fit()
        lm_coeffs[i, :] = model.params
        ####
        slopes_lists = lm_coeffs[:, 1]
        median_slope = np.median(slopes_list)
        mad_slope = np.median(np.abs(slopes_lists - median_slope))
	    if mad_slope == 0:
            mad_slope = 1e-7
	    z_score = (slopes_list - median_slope)/mad_sllope
	    outlier_ind_up = np.where(z_score > 3)[0]
	    outlier_ind_down = np.where(z_score < -3)[0]
	    outlier_ind_up_map[window_length] = outlier_ind_up
	    outlier_ind_down_map[window_length] = outlier_ind_down
	    if remainder is None:
	    	for k in outlier_ind_up:
	    		asdetect_score[k * window_length: (k+1)*window_length] += 1
	    	for k in outlier_ind_down:
	    		asdetect_score[k * window_length: (k+1)*window_length] -= 1
	    else:
	    	start = int(np.floor(remainder/2))
	    	for k in outlier_ind_up:
	    		asdetect_score[start + k * window_length: start + (k+1)*window_length] += 1
	    	for k in outlier_ind_down:
	    		asdetect_score[start + k * window_length: start + (k+1)*window_length] -= 1
	    slope_dict[window_length] = slopes_list
	########
	asdetect_score_normalize = asdetect_score/len(wls)
	return asdetect_score_normalize, slope_dict







