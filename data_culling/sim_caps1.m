% ---------------------------------------------------------------------------%
% Author: Blake Conrad                                                       %
% Contents: Remote Simulation for Distance Testing on Citrix                 %
% ---------------------------------------------------------------------------%

% Establish the table
% ---------------------------------------------------------------------------%
murders = rand(10,1);
thefts = rand(10,1);
numberOfficersWounded = rand(10,1);
numberOfficersKilled = rand(10,1);
state = {'INDIANA','NEW YORK','CALIFORNIA','INDIANA','GEORGIA','TEXAS','ILLINOIS','CALIFORNIA','INDIANA','ALABAMA'};
state = reshape(state,[10 1]);
city = {'INDY','NYC','LA','INDY','ATL','HOUSTAN','CHICAGO','LA','INDY','MONTGOMERY'};
city = reshape(city,[10 1]);
year = {1,4,5,3,4,1,4,11,23,4};
year = reshape(year,[10 1]);
population = {10000,50000,45000,12000,37500,10000,50000,45000,12000,37500};
population = reshape(population,[10 1]);
% ---------------------------------------------------------------------------%


% Create the Table
% ---------------------------------------------------------------------------%
T = table(city,state,year,population,murders,thefts,numberOfficersWounded,numberOfficersKilled);
summary(T)
colnames = T.Properties.VariableNames;
numrows = height(T);
numcols = width(T);
unique_cities = unique(T.city);
unique_states = unique(T.state);
% ---------------------------------------------------------------------------%


% Get Specific Indicies
% ---------------------------------------------------------------------------%
row_index_hits = strcmp(T.city(:),'INDY') & strcmp(T.state(:), 'INDIANA');
iht = T(row_index_hits,:);
% ---------------------------------------------------------------------------%

% Get Summary Statistics
% ---------------------------------------------------------------------------%
ST = grpstats(iht(:,5:8));
ST
% ---------------------------------------------------------------------------%

% Side by side Bar Graph
% ---------------------------------------------------------------------------%
% figure(82);
% bar(ST.mean_murders,ST.mean_thefts,'grouped');
% title('Grouped bar chart');
% ---------------------------------------------------------------------------%

% Generic plots with best fit functions -- and coloring -- works!
% ---------------------------------------------------------------------------%
plt = plot(T.murders); hold on;
plt.Color = 'blue';
x = plt.XData;
y = plt.YData;
[u,v] = prepareCurveData(x,y);
f = fit(u,v,'smoothingspline');
title('Murders from 1980 to 2014 and best fit curve');
plt_f = plot(f,u,v);
plt_f(1).Color = 'black';
plt_f(2).Color = 'red';
xlabel('# of murders');
ylabel('freq of # murders'); hold off;

% The skewness function on a variable -- works!
% rets = [];
% for i=5:8
% rets(i) = skewness(T.(i));
% end
% rets = rets(5:8);
% rets

% Data Splitting -- might not be helpful!
% Using groups to create a table of the mean murder of each city -- works!
% May or may not be useful though.
% [G, murders] = findgroups(T.city);
% apts = splitapply(@mean,T.murders,G);
% T1 = table(apts(1), apts(2), apts(3), apts(4), apts(5), apts(6), apts(7));
% T1.Properties.VariableNames = transpose(murders);
% T1

%---- Kmeans -- works!
[idx,C] = kmeans(table2array(T(:,5:8)),2);
D_NUM_CLUSTERS = length(unique(idx));
cls1_idx =idx(:) == 1; %cluster1 indices
cls2_idx =idx(:) == 2; %cluster2 indices
cls3_idx =idx(:) == 3; %cluster3 indices
cls1_obs = T(cls1_idx,:); %cluster1 observations
cls2_obs = T(cls2_idx,:); %cluster1 observations
cls3_obs = T(cls3_idx,:); %cluster1 observations
cls1_yrs = cls1_obs.year; %cluster1 years
cls2_yrs = cls2_obs.year; %cluster1 years
cls3_yrs = cls3_obs.year; %cluster1 years


% Best clustering visualization I've found - workes!
% figure;
% plot(table2array(T(idx==1,5)),table2array(T(idx==1,6)),'r.','MarkerSize',12);
% hold on;
% plot(table2array(T(idx==2,5)),table2array(T(idx==2,6)),'b.','MarkerSize',12);
% plot(C(:,1),C(:,2),'kx','MarkerSize',15,'LineWidth',3);
% legend('Cluster 1','Cluster 2','Centroids','Location','NW');
% title 'Cluster Assignments and Centroids';
% ylabel('murders');
% xlabel('thefts');
% hold off;

% print all the cities in the first cluster
cls1_cities=unique(T(idx==1,'city'));

% print all the cities in the second cluster
cls2_cities=unique(T(idx==2,'city'));

% thus, grouping cities with similar crime patterns
disp('Cities with similar crime patterns are:\n')
cls1_cities
disp('and:\n')
cls2_cities

% Since you're using MATLAB, look into their "cluster validity" methods, for which
% the optimal number (i.e. KK) can be identified. The silhouette index is one of the
% better metrics, and for a single dataset, there will be a silhouette index value
% for each value of KK (number of clusters) which is preset by the user prior to the analysis.

% Convert a table's columns (homogeneous) to matrix -- works!
% Plot crime patterns for a city ....
% Crime patterns are clusters with each observation at...
% idx==1,idx==2,...idx==N

% I WANT A TIMELINE OVER A CITY's crime patterns (only in 1 cluster)
% # of crimes of a city in a year in their cluster
% So I want to plot amount of crime vs. crime in a city in a cluster in that
% year
% histc(valueList,uniqueValue)
obs1 = T(idx==1,{'city','year'}) %city -- year of cluster 1
T(strcmp(T.city(:),'INDY'),:) %crime in city TOTAL
T(strcmp(T.city(:),'INDY') & idx==1,:) %crime in city in cluster1 only

% Do the side by side plot iteratively
figure;
for i = 1:D_NUM_CLUSTERS
    % Let's plot just crime in cluster 1 to see what patterns were
    uniqueCities = unique(T.city(idx==i));
    numberUniqueCities = length(uniqueCities);
    x = T.city(idx==i)

    % For every city -- count how many times it appears in a crime cluster
    y = zeros(numberUniqueCities);
    for j = 1:numberUniqueCities
        y(j) = sum(strcmp(x,x(j)));
    end
    y=y(:,1);

    % Plot!
    subplot(3,2,i)
    bar(y);
    ylim([0 numberUniqueCities+1]);
    set(gca,'Xtick',1:numberUniqueCities,'XTickLabel',uniqueCities, 'Color','w');
end

%--------------------------------------------------------------------------------%
% Plot of a City and its # of murder each year
% murders over time for a city
% x-axis: time frame: 1-35
% y-axis: number of murders that year
% label: number of Murders per year for Indy
% get all observations including indy as their city
%--------------------------------------------------------------------------------%
indy_rows = strcmp(T.city(:),'INDY');
arg1_murders = table2array(T(indy_rows,'murders'));
arg2_years = T(indy_rows,'year');
arg2_years = cell2mat(arg2_years.(1));
figure();
bar(arg2_years, arg1_murders)
title('Murders in Indy from 1980-2014')
set(gca,'xtick', [1:35], 'xticklabel', [1980:2014], 'Color', 'w')

% subplot(3,1,1)
% hist(y,numberUniqueCities)
% h = findobj(gca,'Type','patch');
% h.FaceColor = [0.75 0.75 0.75];
% h.EdgeColor = 'w';
% subplot(3,1,2)
% hist(y2,numberUniqueCities2)
% h2 = findobj(gca,'Type','patch');
% h2.FaceColor = [0.25 0.25 0.25];
% h2.EdgeColor = 'w';

% plot(table2array(T(idx==1,5)),table2array(T(idx==1,6)),'r.','MarkerSize',12);
% hold on;
% plot(table2array(T(idx==2,5)),table2array(T(idx==2,6)),'b.','MarkerSize',12);
% plot(C(:,1),C(:,2),'kx','MarkerSize',15,'LineWidth',3);
% legend('Cluster 1','Cluster 2','Centroids','Location','NW');
% title 'Cluster Assignments and Centroids';
% ylabel('murders');
% xlabel('thefts');

% hold off;

% ------------------------------------------------------------------------ %
% Create a plot to illustrate crime of each variable over each year given  %
% ------------------------------------------------------------------------ %

% -------------------------------------------- %
% Check our numeric variables for correlations %
% -------------------------------------------- %

