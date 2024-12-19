document.addEventListener("DOMContentLoaded", () => {
    const links = document.querySelectorAll(".sidebar a");
    const contentDiv = document.getElementById("content");

    // Configure marked with syntax highlighting
    marked.setOptions({
        highlight: function (code, lang) {
            if (hljs.getLanguage(lang)) {
                return hljs.highlight(code, { language: lang }).value;
            }
            return code; // Plain text if language not found
        },
    });

    // Load Markdown content dynamically
    const loadMarkdown = async (file) => {
        try {
            const response = await fetch(file);
            if (!response.ok) throw new Error(`Error loading file: ${file}`);
            const markdown = await response.text();
            const html = marked(markdown);
            contentDiv.innerHTML = html;

            // Force Highlight.js to highlight all code blocks
            hljs.highlightAll();
        } catch (err) {
            contentDiv.innerHTML = `<p>Error: ${err.message}</p>`;
        }
    };

    // Add event listeners to menu links
    links.forEach((link) => {
        link.addEventListener("click", (e) => {
            e.preventDefault();
            const file = link.getAttribute("data-md");
            if (file) {
                loadMarkdown(file);
            }
        });
    });

    // Load default content
    loadMarkdown("domesticcat.md");
});