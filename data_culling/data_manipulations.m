% --------+---------+---------------+-----------+----------+ %
% Author: Blake Conrad                                       %
% Contents: Data Science Project                             %
% Data: FBI Crime Data from 1980-2014                        %
% --------+---------+---------------+-----------+----------+ %

% ---------+----------------+--------------------+---------- %
% Structure of the Data Set.
% --------+---------+---------------+-----------+----------+ %
%                                                            %
% S =                                                        %
%     X: [302595x10 double] <-- [302595x10] 10 col           %
%     G: [302595x1 double]  <-- [302595x1] 1 col             %
%     A: {1x302595 cell}    <-- [1x302595] 1 row             %
%     S: {1x302595 cell}    <-- [1x302595] 1 row             %
%     P: [1x302595 double]  <-- [1x302595] 1 row             %
%                                                            %
% --------+---------+---------------+-----------+----------+ %

% --------+---------+---------------+-----------+----------+ %
% Structure of the Variables
% --------+---------+---------------+-----------+----------+ %
%                                                            %
% A (City)| G (Year)| P (Population)| S (States)| X (Crime)| %
%               ^                                   ^        %
%          1-35 -> 1980-2014                        ^        %
%                                                   ^        %
%                                                (cols for X)%
%                                                   ^        %
%   0         1           2           3           4          %
% murder|manslaughter|rapetotal|roberytotal|assaulttotal     %
%                                                            %
%   5               6           7           8                %
% burglarytotal|larcenytotal|autotheft|numberOfficersKilled  %
%                                                            %
%   9                                                        %
% numberOfficersAssaulted                                    %
%                                                            %
% --------+---------+---------------+-----------+----------+ %

% ---------+----------------+--------------------+---------- %
% MATLAB Initialization Commands
% ---------+----------------+--------------------+---------- %
clear variables;
close all;
clc;
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Load Data
% ---------+----------------+--------------------+---------- %
S = load('1980_2014_forBlake.mat');
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Reshape vectors to [302595x1] then concat them into a table
% ---------+----------------+--------------------+---------- %
city                    = reshape(S.A,[302595 1]);
state                   = reshape(S.S,[302595 1]);
year                    = S.G;
population              = reshape(S.P,[302595 1]);
murder                  = S.X(:,1);
manslaughter            = S.X(:,2);
rapetotal               = S.X(:,3);
roberytotal             = S.X(:,4);
assaulttotal            = S.X(:,5);
burglarytotal           = S.X(:,6);
larcenytotal            = S.X(:,7);
autotheft               = S.X(:,8);
numberOfficersKilled    = S.X(:,9);
numberOfficersAssaulted = S.X(:,10);
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% New Tabular Data Format
% +--------+----------------+--------------------+----------+%
% | city | state | year | population | .... crimeStats ..... |
% +--------+----------------+--------------------+----------+%
T = table(city,state,year,population,murder,manslaughter,rapetotal,roberytotal,assaulttotal,burglarytotal,larcenytotal,autotheft,numberOfficersKilled,numberOfficersAssaulted);
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Data Manipulation in a Table by Condition
% ---------+----------------+--------------------+---------- %
log_indices =  strfind(T.city,'INDIANAPOLIS');
act_indices = find(not(cellfun('isempty', log_indices)));
disp(T(act_indices,:));
row_hits = strcmp(T.city(:),'INDIANAPOLIS') & strcmp(T.state(:), '13');
hits_table = T(row_hits,:);
hits_table(1:10,:);
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Group Statistics
% ---------+----------------+--------------------+---------- %
ST = grpstats(T(:,5:14));
ST
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Constants
% ---------+----------------+--------------------+---------- %
colnames = T.Properties.VariableNames;
numrows = height(T);
numcols = width(T);
start_crime_stats_index = 5;
end_crime_stats_index = 14;
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Create a Plot for Frequency vs. Crime over All Variables
% ---------+----------------+--------------------+---------- %
figure();
for i=start_crime_stats_index:end_crime_stats_index
    subplot(3,4,i-3);
    plot(T.(i));
    name = T.Properties.VariableNames(i);
    xlabel(name);
    ylabel('frequency');
    title(name);
end
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Generic Plot for Each Observations Murder
% ---------+----------------+--------------------+---------- %
% figure;
% plt = plot(T.murder(1:1000)); hold on;
% plt.Color = 'blue';
% x = plt.XData;
% y = plt.YData;
% [u,v] = prepareCurveData(x,y);
% f = fit(u,v,'smoothingspline');
% title('Murders at each observation in the data set');
% plt_f = plot(f,u,v);
% plt_f(1).Color = 'black';
% plt_f(2).Color = 'red';
% xlabel('Number of murders');
% ylabel('freq of of a given number of murders'); hold off;
% ---------+----------------+--------------------+---------- %

% ---------+----------------+--------------------+---------- %
% Best Fit Curves for Crime in Indianapolis from 1980-2014
% Over Each Crime Statistic Variable
% ---------+----------------+--------------------+---------- %
figure();
for i = start_crime_stats_index:end_crime_stats_index
    % Get Observations Containing Indianapolis
    indy_rows = strcmp(T.city(:),'INDIANAPOLIS');

    % Get Crime Specifically for Indianapolis
    arg1_crime = table2array(T(indy_rows,i));

    % Get Cooresponding Years that Matched the Crime
    arg2_years = T(indy_rows,'year');

    % Convert the Table Object to a Matrix of Doubles
    arg2_years = [arg2_years.(1)];

    % Begin Subploting in a 2x5 plot-space
    subplot(2,5,i-4);

    % Instantiate a Bar Graph Object for Customization
    plt = bar(arg2_years, arg1_crime); hold on;

    % Create a Best Fit Line (Curve)
    x = plt.XData;
    y = plt.YData;
    [u,v] = prepareCurveData(x,y);
    f = fit(u,v,'smoothingspline');

    % Instantiate the Best Fit Line (Curve) Object for Customization
    plt_f = plot(f,u,v);

    % Customize the Best Fit Line (Curve) Object
    plt_f(1).Color = 'black';
    plt_f(2).Color = 'red';

    % Labels, Titles, and Set Axis to Years (Crime Amt vs. Years Bar Graph)
    xlabel('Year (1980-2014)');
    ylabel('Number of murders');
    title(strcat(colnames(i),' in Indianapolis (1980 to 2015)'))
    set(gca,'xtick', 1:35, 'xticklabel', 1980:2014, 'Color', 'w'); hold off;
end
% ---------+----------------+--------------------+---------- %


% --------------------------------------------------------------------------------- %
% — Thought:
%   What qualifies the 15 different ranks of crime patterns?
%   Possibly consider the rate of change from one cluster to another?
% --------------------------------------------------------------------------------- %

% --------------------------------------------------------------------------------- %
% 1.
% Calculate summary statistics to get an idea of the center/shape and
% range/variability of each variable.
% 2.
% Create individual histograms to assess the shape and look for
% normality/skewness/outliers.
% 3.
% Scatter Plots (to identify correlated variables)
% Look at scatter plots for correlation between the variables
% 4.
% Clustering. (15)
% Then once you start having an idea of the relationships between the variables,
% begin working on clustering...then potential new variables or prediction.



% Next Steps in the Project
% --------------------------------------------------------------------------------- %
% 1. Get good plots for each variable and it's frequency with it's best fit curve.
% 2. Identify the skewness from the curves, and make sense of this.
% 3. Look into 'crime patterns', so cluster based on T(:,5:14) then visualize those clusters based on city/year to see if things have changed in a city over time, and we will then determine if a city is 'healthy' through another classification algorithm or common sense classifying individually..