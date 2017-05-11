x1 = [1,2,3,2,3,1,4,5,7,5];
x2 = [2,3,4,5,4,3,2,1,2,6];
y = [[1,1,1,1,1,1,1,1,1,1]; [2,2,2,2,2,2,2,2,2,2]];
x=[xx';yy'];

uniqueclusters =unique(x);
numuniqueclusters = length(uc);
A=zeros(2,numuniqueclusters);

for i=1:ncla % 1..2
    for j=1:nclu
        A(i,j) = 
    end
end


sum(y == 1 & x == uniqueclusters(1))