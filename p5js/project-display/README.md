# 🎬 Projection

A modern static site generator that creates beautiful, interactive galleries to showcase your coding projects. Built with vanilla JavaScript and modern CSS, featuring search, filtering, dark theme, and responsive design.

## ✨ Features

- **📱 Responsive Design** - Works perfectly on desktop, tablet, and mobile
- **🔍 Search & Filter** - Real-time search and tag-based filtering
- **🌙 Dark Theme** - Modern dark color scheme
- **⭐ Featured Projects** - Highlight your best work
- **🖼️ Background Images** - Project thumbnails as card backgrounds
- **🏷️ Tagging System** - Organize projects with flexible tags
- **📊 Multiple Formats** - Support for both YAML and JSON configuration
- **🚀 Fast Generation** - Efficient static site generation
- **🎨 No Framework Dependencies** - Pure HTML, CSS, and JavaScript

## 🚀 Quick Start

### Prerequisites
- Node.js 14.0.0 or higher

### Installation

```bash
git clone https://github.com/quasarbright/p5js/tree/master/project-display
cd project-display
npm install
```

### Usage

1. **Create your projects file** (YAML preferred, JSON supported):
   ```bash
   # Copy the example and edit
   cp projects.yaml my-projects.yaml
   ```

2. **Generate the site**:
   ```bash
   npm run build
   ```

3. **Serve locally**:
   ```bash
   npm run serve
   ```

4. **Development workflow**:
   ```bash
   npm run dev  # Build and serve
   ```

## 📋 Available Scripts

- `npm run build` - Generate the static site
- `npm run start` - Alias for build
- `npm run serve` - Serve the generated site locally on port 8080
- `npm run dev` - Build and serve (perfect for development)
- `npm run clean` - Remove the dist directory
- `npm run rebuild` - Clean and build

## 📁 Project Structure

```
projection/
├── assets/           # Static assets (favicon, etc.)
├── dist/            # Generated site (created by build)
├── scripts/         # JavaScript source files
├── styles/          # CSS source files
├── projects.yaml    # Your projects data (YAML format)
├── projects.json    # Alternative JSON format
├── generator.js     # Main generator script
└── package.json     # NPM configuration
```

## 🛠️ Configuration

### Projects File Format

Projection supports both YAML (preferred) and JSON formats. The generator will look for files in this order:
1. `projects.yaml`
2. `projects.yml` 
3. `projects.json`

#### YAML Example:
```yaml
config:
  title: "My Projects"
  description: "A showcase of my coding projects"
  baseUrl: "https://username.github.io/"
  itemsPerPage: 20

projects:
  - id: "awesome-project"
    title: "Awesome Project"
    description: "This project does amazing things"
    creationDate: "2024-01-15"
    tags: ["web", "javascript", "featured"]
    pageLink: "./awesome-project/"
    sourceLink: "https://github.com/username/awesome-project"
    thumbnailLink: "./screenshots/awesome.png"
    featured: true
```

#### JSON Example:
```json
{
  "config": {
    "title": "My Projects",
    "description": "A showcase of my coding projects",
    "baseUrl": "https://username.github.io/",
    "itemsPerPage": 20
  },
  "projects": [
    {
      "id": "awesome-project",
      "title": "Awesome Project",
      "description": "This project does amazing things",
      "creationDate": "2024-01-15",
      "tags": ["web", "javascript", "featured"],
      "pageLink": "./awesome-project/",
      "sourceLink": "https://github.com/username/awesome-project",
      "thumbnailLink": "./screenshots/awesome.png",
      "featured": true
    }
  ]
}
```

### Field Descriptions

#### Config Object:
- `title`: Site title displayed in header
- `description`: Site description and meta description
- `baseUrl`: Base URL for resolving relative links
- `itemsPerPage`: Number of projects per page (currently unused)

#### Project Object:
- `id`: Unique identifier (required)
- `title`: Project title (required)
- `description`: Brief project description (required)
- `creationDate`: ISO date string (required)
- `tags`: Array of tags for filtering (required)
- `pageLink`: Link to project page (required)
- `sourceLink`: Link to source code (optional)
- `thumbnailLink`: Link to project screenshot (optional)
- `featured`: Boolean to highlight special projects (optional)

### Path Resolution

The `baseUrl` in config is used to resolve relative paths:

- **Relative paths** (`./path`, `../path`, `filename`) are resolved relative to `baseUrl`
- **Absolute paths** (`/path`) are used as-is
- **Full URLs** (`https://...`) are used as-is

## 🎨 Customization

### Styling
- Edit files in `styles/` directory
- `main.css` - Layout, theme, components
- `cards.css` - Project card styling
- `modal.css` - Modal styling (currently unused)

### Functionality
- Edit files in `scripts/` directory
- `search.js` - Search functionality
- `filter.js` - Filtering and sorting
- `modal.js` - Modal functionality (currently unused)

## 🚢 Deployment

The generated `dist/` directory contains everything needed for deployment:

```bash
npm run build
# Upload dist/ contents to your web server
```

Works with:
- GitHub Pages
- Netlify
- Vercel
- Any static hosting service

## 📈 Project Statistics

- **3,694 lines** of generated code
- **76 projects** in example dataset
- **Modern JavaScript** (ES6+)
- **CSS Grid & Flexbox** layouts
- **Mobile-first** responsive design

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test locally with `npm run dev`
5. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details

## 🔗 Links

- [Live Demo](https://quasarbright.github.io/p5js/project-display/)
- [Source Code](https://github.com/quasarbright/p5js/tree/master/project-display)
- [Author's Projects](https://quasarbright.github.io/)

---

Built with ❤️ by [Mike Delmonaco](https://quasarbright.github.io/) 