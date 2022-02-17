# CREATE DATABASE PortfolioProject;

/*
Covid 19 Data Exploration 

Skills used: Joins, CTEs,Temp Tables, Windows Functions, Aggregate Functions, Creating Views, Converting Data Types

*/

Select * From PortfolioProject.Covidcdths
Where continent is not null 
order by 3,4;

Select Location, date, total_cases, new_cases, total_cdths, population
From PortfolioProject.Covidcdths
Where continent is not null 
order by 1,2;


-- Total Cases vs Total cdths
Select Location, date, total_cases,total_cdths, (total_cdths/total_cases)*100 as cdthPercentage
From PortfolioProject.Covidcdths
Where location like '%states%'
and continent is not null 
order by 1,2;



-- Shows what percentage of population infected with Covid
Select Location, date, Population, total_cases,  (total_cases/population)*100 as PercentPopulationInfected
From PortfolioProject.Covidcdths
Where location like '%states%'
order by 1,2;


-- Countries with Highest Infection Rate compared to Population
Select Location, Population, MAX(total_cases) as HighestInfectionCount,  Max((total_cases/population))*100 as PercentPopulationInfected
From PortfolioProject.Covidcdths
Group by Location, Population
order by PercentPopulationInfected desc;


-- Countries with Highest cdth Count per Population
Select Location, MAX(CAST(total_cdths as decimal)) as TotalcdthCount
From PortfolioProject.Covidcdths
Where continent is not null and location!= "High income"
Group by Location
order by TotalcdthCount desc;

-- Showing contintents with the highest cdth count per population
Select continent, MAX(cast(total_cdths as decimal)) as TotalcdthCount
From PortfolioProject.Covidcdths
Where continent is not null 
Group by continent
order by TotalcdthCount desc;

-- GLOBAL NUMBERS
Select SUM(new_cases) as total_cases, SUM(cast(new_cdths as decimal)) as total_cdths, SUM(cast(new_cdths as decimal))/SUM(New_Cases)*100 as cdthPercentage
From PortfolioProject.Covidcdths
where continent is not null 
Group By date
order by 1,2;

-- Join Covidcdths with Covidcvcinations
-- Shows culmulative population and Percentage of Population that has recieved Covid cvcines
Select cd.continent, cd.location, cd.date, cd.population, cv.new_cvcinations, 
SUM(CONVERT(cv.new_cvcinations, decimal)) OVER (Partition by cd.Location Order by cd.location, cd.Date) as RollingPeoplecvcinated, (RollingPeoplecvcinated/population)*100
From PortfolioProject.Covidcdths cd
Join PortfolioProject.Covidcvcinations cv
	On cd.location = cv.location
	and cd.date = cv.date
where cd.continent is not null 
order by 2,3;


-- Using CTE to perform Calculation on Partition By in previous query
With Popvscv (Continent, Location, Date, Population, New_cvcinations, RollingPeoplecvcinated)
as(
Select cd.continent, cd.location, cd.date, cd.population, cv.new_cvcinations, 
SUM(CONVERT(cv.new_cvcinations, decimal)) OVER (Partition by cd.Location Order by cd.location, cd.Date) as RollingPeoplecvcinated
From PortfolioProject.Covidcdths cd
Join PortfolioProject.Covidcvcinations cv
	On cd.location = cv.location
	and cd.date = cv.date
where cd.continent is not null 
)
Select *, (RollingPeoplecvcinated/Population)*100 as cvcinationPercentage
From Popvscv;
