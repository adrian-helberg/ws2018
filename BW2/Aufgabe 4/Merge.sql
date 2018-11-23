MERGE INTO DIVISION dest
USING (SELECT * FROM ach053.oltp_division) src
ON (dest.ID = src.DIVID)
WHEN NOT MATCHED THEN
    INSERT (ID, Description)
    VALUES (src.DIVID, src.Div_Description);

MERGE INTO PRODUCTCATEGORY dest
USING (SELECT * FROM ach053.oltp_productcategory) src
ON (dest.ID = src.PCID)
WHEN NOT MATCHED THEN
    INSERT (ID, Division_ID, Description)
    VALUES (src.PCID, src.DIVID, src.PC_Description);

MERGE INTO PRODUCT dest
USING (SELECT * FROM ach053.oltp_product) src
ON (dest.ID = src.ProductID)
WHEN NOT MATCHED THEN
    INSERT (ID, ProductCategory_ID, Description, SalesPrice, PurchasePrice)
    VALUES (src.ProductID, src.PCID, src.ProductDescr, src.SalesPrice, src.PurchasePrice);
    
MERGE INTO SALESORGANISATION dest
USING (SELECT * FROM ach053.oltp_salesorg) src
ON (dest.ID = src.SOID)
WHEN NOT MATCHED THEN
    INSERT (ID, Description)
    VALUES (src.SOID, src.SO_Description);
    
MERGE INTO CUSTOMER dest
USING (SELECT * FROM ach053.oltp_customer) src
ON (dest.ID = src.CustomerID)  
WHEN NOT MATCHED THEN
    INSERT (ID, SalesOrganaisation_ID, Description, City, Country)
    VALUES (src.CustomerID, src.SOID, src.CustomerDescr, src.City, src.Country);
    
MERGE INTO SALESDATA dest
USING (
    SELECT
        CUSTOMERID AS CustomerID,
        ORDERDATE AS OrderDate,
        PRODUCTID AS ProductID,
        SUM(SalesQuantity) AS SalesQuantity,
        SUM(SalesQuantity * SalesPrice) AS Revenue,
        SUM(SalesQuantity * ach053.oltp_product.PURCHASEPRICE) AS CostGoodsSold,
        SUM(Discount) AS Discount,
        SUM(SalesQuantity * ach053.oltp_product.SALESPRICE - ach053.oltp_orderitem.DISCOUNT) AS NetSales
    FROM ach053.oltp_orderhead
    JOIN ach053.oltp_orderitem USING (OrderNumber)
    JOIN ach053.oltp_product USING (ProductID)
    GROUP BY OrderDate, ProductID, CustomerID
) src
ON (dest.Product_ID = src.ProductID AND dest.Customer_ID = src.CustomerID AND dest."Time" = src.OrderDate)
WHEN NOT MATCHED THEN
    INSERT (Product_ID, Customer_ID, "Time", Revenue, SalesQuantity, CostGoodsSold, Discount, NetSales)
    VALUES (
        src.ProductID, 
        src.CustomerID,
        src.OrderDate, 
        src.Revenue, 
        src.SalesQuantity, 
        src.CostGoodsSold, 
        src.Discount,
        src.NetSales
    );