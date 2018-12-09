select 
    country, 
    extract(year from "Time") AS year,
    sum(salesquantity) AS quantity, 
    sum(netsales) AS netsales
from salesdata
join customer on salesdata.customer_id = customer.id
join product on salesdata.product_id = product.id
group by country, extract(year from "Time")
order by netsales desc;