// Define endpoint
const baseURL = "http://127.0.0.1:5000/";

// Get references to elements
const inputData = document.getElementById("inputData");
const submitBtn = document.getElementById("submitBtn");
const resetBtn = document.getElementById("resetBtn");
const outputDiv = document.getElementById("output");

// Function to reset the input box and output
resetBtn.addEventListener("click", () => {
    inputData.value = "";
    outputDiv.style.display = "none";
    outputDiv.innerHTML = "";
});

// Function to submit data
submitBtn.addEventListener("click", async () => {
    const data = inputData.value.trim();
    const selectedDay = daySelect.value; // Get selected day value
    const endpoint = `${baseURL}${selectedDay}`; // Construct endpoint URL

    // Clear previous output
    outputDiv.style.display = "none";
    outputDiv.innerHTML = "";

    if (!data) {
        outputDiv.style.display = "block";
        outputDiv.innerHTML = `<p class="error">Input data cannot be empty.</p>`;
        return;
    }

    try {
        // Create a Blob object from the input data
        const fileBlob = new Blob([data], { type: "text/plain" });

        // Use FormData to send the file with a name
        const formData = new FormData();
        formData.append("file", fileBlob, "file");

        // Send POST request
        const response = await fetch(endpoint, {
            method: "POST",
            body: formData
        });

        const result = await response.json();

        if (response.ok) {
            // On success, display Part 1 and Part 2 answers
            outputDiv.style.display = "block";
            outputDiv.innerHTML = `
                <p><strong>Part 1:</strong> ${result.part1}</p>
                <p><strong>Part 2:</strong> ${result.part2}</p>
            `;
        } else {
            // On error, display the error message
            outputDiv.style.display = "block";
            outputDiv.innerHTML = `<p class="error">${result.error}</p>`;
        }
    } catch (error) {
        // Handle network or unexpected errors
        outputDiv.style.display = "block";
        outputDiv.innerHTML = `<p class="error">An unexpected error occurred: ${error.message}</p>`;
    }
});
