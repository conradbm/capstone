% --------+---------+---------------+-----------+----------+ %
% Author: Blake Conrad                                       %
% Contents: Data Science Capstone                            %
% Data: FBI Crime Data from 1980-2014                        %
% File: purity.m                                             %
% Purpose: Get the highest purity score for Kmeans 5..25     %
% --------+---------+---------------+-----------+----------+ %

% --------+---------+---------------+-----------+----------+ %
% The following code snippet opens all 5 of my Kmeans runs
% Then gets the purity score
% --------+---------+---------------+-----------+----------+ %

% [num txt] = xlsread('baseline_k_trials/k5.csv');
% [num txt] = xlsread('baseline_k_trials/k10.csv');
% [num txt] = xlsread('baseline_k_trials/k15.csv');
% [num txt] = xlsread('baseline_k_trials/k20.csv');
[num txt] = xlsread('baseline_k_trials/k25.csv');


% Get all numeric cells from the first 50 rows into a list called x1
x1=[];
for i=1:50
tmp=num(i+1,:);
x1=[x1 tmp(isfinite(tmp))];
x1
end
x1

% get all numeric cells from the second 50 rows into a list called x2
x2=[];
for i=51:100
tmp=num(i+1,:);
x2=[x2 tmp(isfinite(tmp))];
end
x2

% Make one column vector x
% x1 <-> x(1:length(x1,1)
% x2 <-> x(length(x1)+1:length(x2),1)
% 
x=[x1';x2'];
x

% Make a replica of x called y s.t.
% y1 <-> y(1:length(x1,1)) --> column of 1's
% y2 <-> y(length(x1)+1:length(x2),1) --> column of 2's
% 
y=[ones(length(x1),1);2*ones(length(x2),1)];
y

% Get a list of all unique clusters
uc=unique(x);

% Number of cluster length unique
nclu=length(uc);
ncla=2;

% A has the followed shape
% 
%             Number of unique clusters cols
%            __|__|__|__|__|__|__|__|__|__|... <- col per cluster found
%  best 50  {
%  worst 50 {
% 
% For the first 50 rows {i.e., x <==> x(:,1:length(x1)) <==> y==1}
% % Sum up how many appearances each unique cluster occurs in X for them.
% For the second 50 rows {i.e., x <==> x(:,length(x1)+1:length(x1)) <==> y==2}
% % Sum up how many appearances each unique cluster occurs in X for them.
% 
% This fills up A with the following form:
% 
% 
%                ncols(uc(x))
%        A = [ sum(y==1 & x==uc(1)), sum(y==1 & x==uc(2)), ...
%  nrows(2)    sum(y==2 & x==uc(1)), sum(y==2 & x==uc(2)), ... ]
% 
% The first row represents the best 50 with each cluster hits in the set
% The second row represents the worst 50 with each cluster hits in the set
% 
% 
A=zeros(ncla,nclu);
for i=1:ncla % 1..2
    for j=1:nclu % 1..uniqueClusterCount
%         if y==1, we sum up all x1 == uc(j)
%         if y==2, we sum up all x2 == uc(j)
        A(i,j)=sum(y == i & x == uc(j));
    end
end

purindex=sum(max(A,[],1))/length(x)
purindex
A

% --------+---------+---------------+-----------+----------+ %
% Final Purity Scores 
% 
% K = 5  | purity-index value = 0.8453
% K = 10 | purity-index value = 0.8905
% K = 15 | purity-index value = 0.8786
% K = 20 | purity-index value = 0.8419
% K = 25 | purity-index value = 0.8688
% 
% We conclude, K=10 has the highest purity and clusters the best
% according to our assumptions set in the 50 best 50 worst cities 
% chosen.
% 
% --------+---------+---------------+-----------+----------+ %

% % % %
% The following tells us which clusters were the 
% best and worst dominated.
% A =
%      2     0   501    81   365     0     0     0    39     0
%     34   257   201   237    28   686    55     1   170    19
% Bad Crime Clusters = {1,2,4,6,7,8,9,10}
% Good Crime Clusters = {3,5}
% 
