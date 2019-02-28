val.list = [100, 200, 500, 1000, 1500];

distro = 'unif';
sim.size = 20;

for ind = 1:4
    varParam = val.list(ind);
    Y = csvread(strcat("dat/data_",distro,"_",num2str(varParam), "_", num2str(i), ".csv"), 1,1);
    skel = csvread(strcat("dat/skeleton_",distro,"_",num2str(varParam), "_", num2str(i), ".csv"),1, 1);
    Adj = sparsePairwise(Y, skel);
    csvwrite(strcat("res/adj_", distro,"_", num2str(varParam),"_",num2str(i), ".csv"), Adj);

    Y = csvread(strcat("dat/data_hub_",distro,"_",num2str(varParam), "_", num2str(i), ".csv"), 1,1);
    skel = csvread(strcat("dat/skeleton_hub_",distro,"_",num2str(varParam), "_", num2str(i), ".csv"),1, 1);
    Adj = sparsePairwise(Y, skel);
    csvwrite(strcat("res/adj_hub_", distro,"_", num2str(varParam),"_",num2str(i), ".csv"), Adj);
end

