SELECT Shippers.ShipperName, COUNT(Orders.OrderID) AS NumberOfOrders FROM Orders
LEFT JOIN Shippers ON Orders.ShipperID = Shippers.ShipperID
GROUP BY ShipperName;

Select Employees.LastName, Count(Orders.OrderID) As NumberOfOrders From Orders
Left Join Employees ON Orders.EmployeeID = Employees.EmployeeID
Group by LastName Order by NumberOfOrders Desc
Limit 3;

Select Products.ProductName, Customers.Country, Count(Orders.OrderID) as NumberOfOrders FROM Customers
Right Join Orders on Customers.CustomerID = Orders.CustomerID
Right Join Order_details on Orders.OrderID = Order_details.OrderID
Right Join Products ON Order_details.ProductID = Products.ProductID
Where Country = 'Germany'
Group by ProductName Order by NumberOfOrders Desc
Limit 5;
