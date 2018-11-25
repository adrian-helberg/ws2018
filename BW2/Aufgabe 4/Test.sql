select 
    country, 
    extract(year from "Time"), 
    sum(salesquantity), 
    sum(netsales) 
from salesdata 
join customer on salesdata.customer_id = customer.id
join product on salesdata.product_id = product.id 
group by country, extract(year from "Time");