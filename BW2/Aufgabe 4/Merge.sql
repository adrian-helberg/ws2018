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
        ach053.oltp_orderhead.ORDERNUMBER OrderNumber,
        ach053.oltp_orderhead.CUSTOMERID CustomerID,
        ach053.oltp_orderhead.ORDERDATE OrderDate,
        ach053.oltp_orderitem.PRODUCTID ProductID,
        ach053.oltp_orderitem.ORDERITEM OrderItem,
        ach053.oltp_product.SALESPRICE SalesPrice,
        ach053.oltp_product.PURCHASEPRICE PurchasePrice,
        ach053.oltp_orderitem.SALESQUANTITY SalesQuantity,
        ach053.oltp_orderitem.DISCOUNT Discount
    FROM ach053.oltp_orderhead 
    INNER JOIN ach053.oltp_orderitem 
    ON ach053.oltp_orderhead.ordernumber = ach053.oltp_orderitem.ordernumber
    INNER JOIN ach053.oltp_product
    ON ach053.oltp_orderitem.PRODUCTID = ach053.oltp_product.productid
) src
ON (dest.ID = src.OrderNumber AND dest.Position = src.OrderItem)
WHEN NOT MATCHED THEN
    INSERT (ID, Product_ID, Customer_ID, Position, "Time", Revenue, SalesQuantity, CostGoodsSold, Discount, NetSales)
    VALUES (
        src.OrderNumber,
        src.ProductID, 
        src.CustomerID,
        src.OrderItem,
        src.OrderDate, 
        src.SalesQuantity * src.SalesPrice, 
        src.SalesQuantity, 
        src.SalesQuantity * PurchasePrice, 
        src.Discount,
        src.SalesQuantity * src.SalesPrice - src.Discount
    );