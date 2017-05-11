% -------------------------------------------------------------- %
% Simple Kmeans clustering example
% Resource found at:
% https://www.mathworks.com/help/stats/k-means-clustering.html
% 
% 
% Notes:
% It is important to realize that the kmeans clustering is going
% to work with numeric data (because we are calculating euclidian
% distances, and examining how far each of N observations falls
% from the mean of others, this is how we determine if it is a 
% good fit for the cluster.
% -------------------------------------------------------------- %

% Don't forget these.
% -------------------------------------------------------------- %
clear variables
close all
clc


% Load the data
rng default  % For reproducibility
load kmeansdata
% -------------------------------------------------------------- %

% Examine the data
% -------------------------------------------------------------- %
size(X)
class(X) % Notice the 'double' matrix type
% -------------------------------------------------------------- %

% Get the clusters
% -------------------------------------------------------------- %
idx3 = kmeans(X,3,'Distance','sqeuclidean');

figure
[silh3,h] = silhouette(X,idx3,'sqeuclidean');
h = gca;
h.Children.EdgeColor = [.25 .5 .25];
xlabel 'Silhouette Value'
ylabel 'Cluster'