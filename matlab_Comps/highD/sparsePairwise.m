function Adj = sparsePairwise(Y, skel)
[~, p] = size(Y);
Adj = zeros(p,p);
for i = 2:p
    for j = 1:i
        if skel(i,j) == 1
            pwRes = pwling(Y(:,[i j])',1);
            if pwRes(1, 2) > 0
                Adj(j,i) = 1;
            else
                Adj(i, j) = 1;
            end
        end
    end
end


    












end


