/*Basic Phone Breakdown Code */

SELECT Phone_Contacts, Total, Employee_Name, Number_of_Hours, (Total/Number_of_Hours) AS Phone_Average
FROM phone_hours
INNER JOIN phone_contacts
ON (SUBSTRING(phone_contacts.Phone_Contacts, 1, 6) ) = SUBSTRING(phone_hours.Employee_Name, 1, 6) ;


/*Basic Email Breakdown Code */
SELECT Email_Contacts, Total, Employee_Name, Number_of_Hours, (Total/Number_of_Hours) AS Email_Average
FROM email_hours
INNER JOIN email_contacts 
ON (SUBSTRING(email_contacts.Phone_Contacts, 1, 6) ) = SUBSTRING(email_hours.Employee_Name, 1, 6) ;



/* Phone Breakdown Code (Create Table ver.) */
CREATE TABLE Phone_Breakdown 
SELECT Phone_Contacts, Total, Employee_Name, Number_of_Hours, (Total/Number_of_Hours) AS Phone_Average
FROM phone_hours
INNER JOIN phone_contacts 
ON (SUBSTRING(phone_contacts.Phone_Contacts, 1, 6) ) = SUBSTRING(phone_hours.Employee_Name, 1, 6) ;

/*Phone Breakdown Code (Clear + Update Table ver.) */
DELETE FROM Phone_Breakdown;
INSERT INTO Phone_Breakdown
SELECT Phone_Contacts, Total, Employee_Name, Number_of_Hours, (Total/Number_of_Hours) AS Phone_Average
FROM phone_hours
INNER JOIN phone_contacts 
ON (SUBSTRING(phone_contacts.Phone_Contacts, 1, 6) ) = SUBSTRING(phone_hours.Employee_Name, 1, 6) ;

/* Finding Top 10 (Phones)*/
SELECT Phone_Contacts AS "Name", ROUND(Phone_Average, 2) AS "Number of Calls Per Hour"
FROM Phone_Breakdown
ORDER BY Phone_Average DESC
LIMIT 10;

/*Email Breakdown Code (Create Table ver.)*/
CREATE TABLE Email_Breakdown
SELECT Email_Contacts, Total, Employee_Name, Number_of_Hours, (Total/Number_of_Hours) AS Email_Average
FROM email_hours
INNER JOIN email_contacts
ON (SUBSTRING(email_contacts.Email_Contacts, 1, 6) ) = SUBSTRING(email_hours.Employee_Name, 1, 6) ;

/* Email Breakdown Code (Clear + Update Table ver.)*/
DELETE FROM Email_Breakdown;
INSERT INTO Email_Breakdown
SELECT Email_Contacts, Total, Employee_Name, Number_of_Hours, (Total/Number_of_Hours) AS Email_Average
FROM email_hours
INNER JOIN email_contacts
ON (SUBSTRING(email_contacts.Email_Contacts, 1, 6) ) = SUBSTRING(email_hours.Employee_Name, 1, 6) ;

/* Finding Top 10 (Email)*/
SELECT Email_Contacts AS "Name", ROUND(Email_Average, 2) AS "Number of Emails Per Hour"
FROM Email_Breakdown
ORDER BY Email_Average DESC
LIMIT 10;

/* Code Notes */
/* Potential code to solve matching erorrs 
"AND phone_contacts.Phone_Contacts LIKE ('%' + SUBSTRING(phone_hours.Employee_Name, -3) + '%')" */