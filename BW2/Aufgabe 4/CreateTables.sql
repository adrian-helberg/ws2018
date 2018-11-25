DROP TABLE SalesData;
DROP TABLE Product;
DROP TABLE ProductCategory;
DROP TABLE Division;
DROP TABLE Customer;
DROP TABLE SalesOrganisation;

CREATE TABLE SalesOrganisation (
    ID VARCHAR(255) NOT NULL CONSTRAINT PK_SalesOrganisation PRIMARY KEY,
    Country_ID VARCHAR(255) NOT NULL CONSTRAINT FK_SalesOrganisation_Country REFERENCES Country(ID),
    Description VARCHAR(255) NULL
);

CREATE TABLE Customer (
    ID INTEGER NOT NULL CONSTRAINT PK_Customer PRIMARY KEY,
    SalesOrganaisation_ID VARCHAR(255) NOT NULL CONSTRAINT FK_Customer_Salesorganisation REFERENCES SalesOrganisation(ID),
    Description VARCHAR(255) NULL,
    City VARCHAR(255) NULL,
    Country VARCHAR(255) NULL
);

CREATE TABLE Division (
    ID VARCHAR(255) NOT NULL CONSTRAINT PK_Division PRIMARY KEY,
    Description VARCHAR(255) NULL
);

CREATE TABLE ProductCategory (
    ID CHAR(10) NOT NULL CONSTRAINT PK_ProductCategory PRIMARY KEY,
    Division_ID VARCHAR(255) NOT NULL CONSTRAINT FK_ProductCategory_Division REFERENCES Division(ID),
    Description VARCHAR(255) NULL
);

CREATE TABLE Product (
    ID CHAR(10) NOT NULL CONSTRAINT PK_Product PRIMARY KEY,
    ProductCategory_ID CHAR(10) NOT NULL CONSTRAINT FK_Product_ProductCategory REFERENCES ProductCategory(ID),
    Description VARCHAR(255) NULL,
    SalesPrice DECIMAL NULL,
    PurchasePrice DECIMAL NULL
);

CREATE TABLE SalesData (
    Product_ID CHAR(10) NOT NULL CONSTRAINT FK_Sales_Product REFERENCES Product(ID),
    Customer_ID INTEGER NOT NULL CONSTRAINT FK_Sales_Customer REFERENCES Customer(ID),
    "Time" DATE NOT NULL,
    Revenue Decimal NULL,
    SalesQuantity INTEGER NULL,
    CostGoodsSold DECIMAL NULL,
    Discount DECIMAL NULL,
    NetSales DECIMAL NULL,
    CONSTRAINT PK_Sales PRIMARY KEY(Product_ID, Customer_ID, "Time")
);