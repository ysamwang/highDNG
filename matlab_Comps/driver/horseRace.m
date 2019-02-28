function res = horseRace(n, dist)
    coefBA = ((rand(1) > .5) * 2 - 1) * unifrnd(.65, 1,1);

    scaleParams = unifrnd(.8, 1, 1,2);
    
    if strcmp(dist,'gamma')
        A = (gamrnd(2, 1/sqrt(2), n,1) - 2 / sqrt(2)) * scaleParams(1);
        B = coefBA * A + (gamrnd(2, 1/sqrt(2), n,1) - 2 / sqrt(2)) * scaleParams(2);
    else
        A = unifrnd(-sqrt(3), sqrt(3), n, 1) * scaleParams(1);
        B = coefBA * A + unifrnd(-sqrt(3), sqrt(3), n, 1) * scaleParams(2);
    end
    

    res = [A B];

    pwRes = pwling(res', 1);
    dlRes = Dlingam(res');

    res = [pwRes(1,2) > 0, dlRes(2,1) ~= 0];
end