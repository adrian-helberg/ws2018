MERGE INTO DIVISION dest
USING (SELECT * FROM ach053.oltp_division) src
ON (dest.ID = src.DIVID)
WHEN MATCHED THEN UPDATE SET
    dest.Description = src.Div_Description
WHEN NOT MATCHED THEN
    INSERT (ID, Description)
    VALUES (src.DIVID, src.Div_Description);

MERGE INTO PRODUCTCATEGORY dest
USING (SELECT * FROM ach053.oltp_productcategory) src
ON (dest.ID = src.PCID)
WHEN MATCHED THEN UPDATE SET
    dest.Division_ID = src.DIVID,
    dest.Description = src.PC_Description
WHEN NOT MATCHED THEN
    INSERT (ID, Division_ID, Description)
    VALUES (src.PCID, src.DIVID, src.PC_Description);

MERGE INTO PRODUCT dest
USING (SELECT * FROM ach053.oltp_product) src
ON (dest.ID = src.ProductID)
WHEN MATCHED THEN UPDATE SET    
    dest.ProductCategory_ID = src.PCID,
    dest.Description = src.ProductDescr,
    dest.SalesPrice = src.SalesPrice,
    dest.PurchasePrice = src.PurchasePrice
WHEN NOT MATCHED THEN
    INSERT (ID, ProductCategory_ID, Description, SalesPrice, PurchasePrice)
    VALUES (src.ProductID, src.PCID, src.ProductDescr, src.SalesPrice, src.PurchasePrice);
    
MERGE INTO COUNTRY dest
USING (SELECT * FROM ach053.oltp_salesorg) src
ON (dest.ID = src.SOID)
WHEN MATCHED THEN UPDATE SET
    dest.Description = src.SO_Description
WHEN NOT MATCHED THEN
    INSERT (ID, Description)
    VALUES (src.SOID, src.SO_Description);
    
MERGE INTO SALESORGANISATION dest
USING (SELECT * FROM ach053.oltp_salesorg) src
ON (dest.ID = src.SOID)
WHEN MATCHED THEN UPDATE SET
    dest.Country_ID = src.SOID,
    dest.Description = src.SO_Description
WHEN NOT MATCHED THEN
    INSERT (ID, Country_ID, Description)
    VALUES (src.SOID, src.SOID, src.SO_Description);
    
MERGE INTO CUSTOMER dest
USING (SELECT * FROM ach053.oltp_customer) src
ON (dest.ID = src.CustomerID)
WHEN MATCHED THEN UPDATE SET
    dest.SalesOrganaisation_ID = src.SOID,    
    dest.Description = src.CustomerDescr,
    dest.City = src.City    
WHEN NOT MATCHED THEN
    INSERT (ID, SalesOrganaisation_ID, Description, City)
    VALUES (src.CustomerID, src.SOID, src.CustomerDescr, src.City);
  
/* TODO */  
MERGE INTO SALES dest
USING (SELECT * FROM ach053.oltp_customer) src
ON (dest.ID = src.OrderNumber)
WHEN MATCHED THEN UPDATE SET
    dest.Product_ID = (SELECT "ID" FROM Product WHERE "ID" = 0)
    dest.Customer_ID = src.CustomerID,
    dest.Time = src.OrderDate,
    dest.Revenue = 0,
    dest.SalesQuantity = 0,
    dest.CostGoodsSold = 0,
    dest.Discount = 0,
    dest.NetSales = 0,
WHEN NOT MATCHED THEN
    INSERT (ID, SalesOrganaisation_ID, Description, City)
    VALUES (src.CustomerID, src.SOID, src.CustomerDescr, src.City);