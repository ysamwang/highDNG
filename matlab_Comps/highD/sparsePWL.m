function ord = sparsePWL(Y, skel)

res = Y;
[~, p] = size(Y);
ord = zeros(1,p);
indices = 1:p;

for v = 1:(p-1)
    pwRes = pwling(res', 1);
    output = zeros(p - v  + 1,1);
    
    for i = 1:len(indices)
        k = indices(i);
        possPA = intersect(find(skel(k,:)),indices);
        output(i) = -sum( bsxfun(@max,pwRes(i, possPA), zeros(1, len(possPA)) ).^2);
    end

    
    [~, root] = max(output);
    indices(root) = []; %delete root from unordered list
    ord(v) = root; % add root to ordered list
    
end
ord(p) = indices(1);
end












end


