document.addEventListener('DOMContentLoaded', function() {
    // Initialize syntax highlighting
    hljs.highlightAll();
    
    // Generate Table of Contents
    generateTOC();
    
    // Initialize theme toggle
    initThemeToggle();
    
    // Add back to top button functionality
    initBackToTop();
    
    // Add code copy functionality
    initCodeCopy();
    
    // Add scroll spy (highlight active TOC item)
    initScrollSpy();
    
    // Add current year to footer
    document.getElementById('current-year').textContent = new Date().getFullYear();
    
    // Add smooth scrolling for anchor links
    initSmoothScrolling();
});

// Generate Table of Contents
function generateTOC() {
    const tocList = document.getElementById('toc-list');
    const sections = document.querySelectorAll('.section');
    
    sections.forEach(section => {
        const h2 = section.querySelector('h2');
        const h3s = section.querySelectorAll('h3');
        
        if (h2) {
            const sectionId = section.id;
            const listItem = document.createElement('li');
            const link = document.createElement('a');
            link.href = `#${sectionId}`;
            link.textContent = h2.textContent;
            link.setAttribute('data-section', sectionId);
            listItem.appendChild(link);
            
            // Add subsections if they exist
            if (h3s.length > 0) {
                const subList = document.createElement('ul');
                h3s.forEach((h3, index) => {
                    const subId = `${sectionId}-sub-${index}`;
                    h3.id = subId;
                    
                    const subItem = document.createElement('li');
                    const subLink = document.createElement('a');
                    subLink.href = `#${subId}`;
                    subLink.textContent = h3.textContent;
                    subLink.setAttribute('data-section', subId);
                    subItem.appendChild(subLink);
                    subList.appendChild(subItem);
                });
                listItem.appendChild(subList);
            }
            
            tocList.appendChild(listItem);
        }
    });
}

// Initialize theme toggle
function initThemeToggle() {
    const toggleSwitch = document.querySelector('.theme-switch input[type="checkbox"]');
    
    // Check for saved theme preference or prefers-color-scheme
    const currentTheme = localStorage.getItem('theme') || 
                         (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light');
    
    // Set initial theme
    if (currentTheme === 'dark') {
        document.documentElement.setAttribute('data-theme', 'dark');
        toggleSwitch.checked = true;
    }
    
    // Listen for theme switch changes
    toggleSwitch.addEventListener('change', function(e) {
        if (e.target.checked) {
            document.documentElement.setAttribute('data-theme', 'dark');
            localStorage.setItem('theme', 'dark');
        } else {
            document.documentElement.setAttribute('data-theme', 'light');
            localStorage.setItem('theme', 'light');
        }
    });
}

// Initialize back to top button
function initBackToTop() {
    const backToTopButton = document.getElementById('back-to-top');
    
    window.addEventListener('scroll', () => {
        if (window.pageYOffset > 300) {
            backToTopButton.style.display = 'block';
        } else {
            backToTopButton.style.display = 'none';
        }
    });
    
    backToTopButton.addEventListener('click', () => {
        window.scrollTo({
            top: 0,
            behavior: 'smooth'
        });
    });
}

// Initialize code copy functionality
function initCodeCopy() {
    const codeBlocks = document.querySelectorAll('pre code');
    
    codeBlocks.forEach((block, index) => {
        const pre = block.parentNode;
        
        // Don't add multiple buttons to the same pre
        if (!pre.querySelector('.copy-btn')) {
            const button = document.createElement('button');
            button.className = 'copy-btn';
            button.textContent = 'Copier';
            button.setAttribute('data-index', index);
            
            pre.appendChild(button);
            
            button.addEventListener('click', () => {
                const code = block.textContent;
                navigator.clipboard.writeText(code)
                    .then(() => {
                        button.textContent = 'Copié !';
                        setTimeout(() => {
                            button.textContent = 'Copier';
                        }, 2000);
                    })
                    .catch(err => {
                        console.error('Erreur de copie :', err);
                        button.textContent = 'Erreur !';
                        setTimeout(() => {
                            button.textContent = 'Copier';
                        }, 2000);
                    });
            });
        }
    });
    
    // Add specific copy button for the complete example
    const copyCompleteExampleBtn = document.getElementById('copyCompleteExample');
    if (copyCompleteExampleBtn) {
        copyCompleteExampleBtn.addEventListener('click', () => {
            const codeBlock = document.querySelector('#completeexample pre code');
            if (codeBlock) {
                const code = codeBlock.textContent;
                navigator.clipboard.writeText(code)
                    .then(() => {
                        copyCompleteExampleBtn.textContent = 'Copié !';
                        setTimeout(() => {
                            copyCompleteExampleBtn.textContent = 'Copier l\'exemple';
                        }, 2000);
                    })
                    .catch(err => {
                        console.error('Erreur de copie :', err);
                        copyCompleteExampleBtn.textContent = 'Erreur !';
                        setTimeout(() => {
                            copyCompleteExampleBtn.textContent = 'Copier l\'exemple';
                        }, 2000);
                    });
            }
        });
    }
}

// Initialize scroll spy
function initScrollSpy() {
    const sections = document.querySelectorAll('.section');
    const navLinks = document.querySelectorAll('.toc a');
    
    window.addEventListener('scroll', () => {
        let current = '';
        
        sections.forEach(section => {
            const sectionTop = section.offsetTop - 100;
            const sectionHeight = section.offsetHeight;
            
            if (window.pageYOffset >= sectionTop && window.pageYOffset < sectionTop + sectionHeight) {
                current = section.getAttribute('id');
            }
        });
        
        navLinks.forEach(link => {
            link.classList.remove('active');
            if (link.getAttribute('data-section') === current) {
                link.classList.add('active');
            }
        });
    });
}

// Initialize smooth scrolling for anchor links
function initSmoothScrolling() {
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function(e) {
            e.preventDefault();
            
            const targetId = this.getAttribute('href').substring(1);
            const targetElement = document.getElementById(targetId);
            
            if (targetElement) {
                window.scrollTo({
                    top: targetElement.offsetTop - 80,
                    behavior: 'smooth'
                });
            }
        });
    });
}

// Add search functionality (bonus feature)
function initSearch() {
    const searchInput = document.getElementById('search-input');
    const searchResults = document.getElementById('search-results');
    
    if (!searchInput || !searchResults) return;
    
    searchInput.addEventListener('input', () => {
        const searchTerm = searchInput.value.toLowerCase();
        
        if (searchTerm.length < 3) {
            searchResults.innerHTML = '';
            searchResults.style.display = 'none';
            return;
        }
        
        // Get all content
        const sections = document.querySelectorAll('.section');
        let results = [];
        
        sections.forEach(section => {
            const title = section.querySelector('h2').textContent;
            const content = section.textContent;
            
            if (title.toLowerCase().includes(searchTerm) || content.toLowerCase().includes(searchTerm)) {
                results.push({
                    id: section.id,
                    title: title,
                    preview: getPreviewText(content, searchTerm)
                });
            }
        });
        
        // Display results
        displaySearchResults(results);
    });
    
    function getPreviewText(content, searchTerm) {
        const maxLength = 100;
        const lowerContent = content.toLowerCase();
        const index = lowerContent.indexOf(searchTerm);
        
        if (index === -1) return content.substring(0, maxLength) + '...';
        
        const start = Math.max(0, index - 40);
        const end = Math.min(content.length, index + searchTerm.length + 40);
        return (start > 0 ? '...' : '') + content.substring(start, end) + (end < content.length ? '...' : '');
    }
    
    function displaySearchResults(results) {
        if (results.length === 0) {
            searchResults.innerHTML = '<p>Aucun résultat trouvé</p>';
        } else {
            let html = '<ul>';
            results.forEach(result => {
                html += `
                    <li>
                        <a href="#${result.id}">${result.title}</a>
                        <p>${result.preview}</p>
                    </li>
                `;
            });
            html += '</ul>';
            searchResults.innerHTML = html;
        }
        
        searchResults.style.display = 'block';
    }
}