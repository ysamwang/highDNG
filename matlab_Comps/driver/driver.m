n_list = [5 10 15 20 30 40 50 75 100];
resOut = zeros(length(n_list), 4);

sim_size = 10000;

for n_ind = 1:length(n_list)
 res_inter = zeros(sim_size, 2);
 for s = 1:sim_size
     res_inter(s, :) = horseRace(n_list(n_ind), 'gamma');
 end
 resOut(n_ind, 1:2) = mean(res_inter);
 
 res_inter = zeros(sim_size, 2);
 for s = 1:sim_size
     res_inter(s, :) = horseRace(n_list(n_ind), 'unif');
 end

 resOut(n_ind, 3:4) = mean(res_inter);
end
resOut

csvwrite('~/Dropbox/highDimDag/highDCode/highDNG/matlab_Comps/lingRes.csv', resOut)