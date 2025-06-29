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
