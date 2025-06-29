# Concurrent-Banking-Application-with-Asynchronous-Message-Passing

<ul>
  <li>Reads customer data from a file, including name and loan objective.</li>
  <li>Reads bank data from a file, including name and available funds.</li>
  <li>Spawns a process for each customer with a specific loan objective.</li>
  <li>Spawns a process for each bank with an initial loan fund.</li>
  <li>Spawns a master logging process to handle all printed output.</li>
  <li>Customers randomly choose a bank and request a loan between 1 and 50 dollars.</li>
  <li>Customers wait between 10–100 milliseconds between loan attempts.</li>
  <li>Banks approve loan requests if they have enough funds.</li>
  <li>Banks deny loan requests if they lack sufficient funds.</li>
  <li>Banks deduct the approved loan amount from their balance.</li>
  <li>Customers keep track of their remaining goal and stop when it is met or no banks remain.</li>
  <li>Customers notify the master process when they finish.</li>
  <li>All logging messages (requests and responses) are handled by the master process.</li>
  <li>The master process waits for all customers to finish before printing the final report.</li>
  <li>The final report shows how much each customer requested and received.</li>
  <li>The final report shows each bank’s original and remaining balance.</li>
  <li>The program shuts down cleanly after displaying the summary.</li>
</ul>

Compile: ``` erlc money.erl customer.erl bank.erl``` - Creates ```money.beam``` ```customer.beam``` ```bank.beam```

Run: ```  erl -noshell -run money start c1.txt b1.txt -s init stop ```

Sample Log:

```
.......
?karen requests a loan of 22 dollar(s) from the nova bank
$ The nova bank approves a loan of 22 dollar(s) to karen
? james requests a loan of 2 dollar(s) from the nova bank
$ The nova bank approves a loan of 2 dollar(s) to james
? james requests a loan of 34 dollar(s) from the nova bank
$ The nova bank denies a loan of 34 dollar(s) to james
? karen requests a loan of 17 dollar(s) from the nova bank
$ The nova bank denies a loan of 17 dollar(s) to karen

** Banking Report **
Customers:
sam: objective 200, received 200
james: objective 300, received 185
karen: objective 200, received 179
-----
Total: objective 700, received 564
Banks:
apple: original 100, balance 29
knox: original 200, balance 4
nova: original 300, balance 3
-----
Total: original 600, loaned 564
```
