# PDF-Maker (MediaWiki to LaTeX Converter)

A powerful tool for converting MediaWiki content to LaTeX and PDF formats.

## Overview

PDF-Maker is based on the MediaWiki to LaTeX project, which allows you to fetch MediaWiki articles (from Wikipedia, Wikibooks, etc.) including all their content recursively (subpages, pictures, etc.) and generate LaTeX documents from them. The tool can automatically compile the LaTeX output to produce high-quality PDFs.

## Features

- Recursive fetching of MediaWiki content
- Conversion of Wiki markup to LaTeX
- Automatic PDF generation
- Customizable templates
- Support for images and tables
- Multiple output formats (PDF, LaTeX, HTML, ePub, ODT)
- Server mode for web-based usage

## Installation

### Dependencies

This application is written in Haskell and requires:
- GHC (Glasgow Haskell Compiler)
- LaTeX distribution (e.g., TexLive or MikTeX) for PDF generation
- Additional runtime dependencies depending on your system

### Building from Source

#### Using Cabal (traditional method)

```bash
# Clone the repository
git clone https://github.com/yourusername/PDF-Maker.git
cd PDF-Maker

# Build using cabal
cabal update
cabal install --only-dependencies
cabal build
```

#### Using Stack (recommended)

```bash
# Clone the repository
git clone https://github.com/yourusername/PDF-Maker.git
cd PDF-Maker

# Build using Stack
stack setup    # Downloads and installs the correct GHC version if needed
stack build    # Builds the project
stack install  # Optional: installs the executable in Stack's local bin path
```

#### Using Docker (easiest)

```bash
# Clone the repository
git clone https://github.com/yourusername/PDF-Maker.git
cd PDF-Maker

# Run using the helper script
./run-docker.sh http://en.wikibooks.org/wiki/Haskell

# Or manually with docker-compose
docker-compose build
docker-compose run --rm pdfmaker http://en.wikibooks.org/wiki/Haskell
```

Output files will be available in the `./output` directory.

## Usage

### Basic Command Line Usage

```bash
mediawiki2latex [OPTIONS] URL
```

Example:
```bash
mediawiki2latex http://en.wikibooks.org/wiki/Haskell
```

### Important Options

- `--output=FILE`: Specify output filename
- `--templates=DIR`: Use custom templates from directory
- `--resolution=DPI`: Set image resolution
- `--paper=FORMAT`: Set paper format (a4, letter, etc.)
- `--html`: Generate HTML output instead of PDF
- `--epub`: Generate ePub output
- `--odt`: Generate ODT output

## Customization

The LaTeX output can be customized using templates. The default templates are located in the `latex/` directory.

## License

This project is licensed under the GNU General Public License v2.0 or later - see the [LICENSE](LICENSE) file for details.

## Credits

Originally developed by Dirk Hünniger.

## Roadmap for Improvements

### Short-term Goals (0-3 months)

1. **Code Modernization**
   - Update Haskell codebase to use more modern idioms
   - Migrate to newer GHC versions
   - Improve error handling and logging

2. **Build System Improvements**
   - ✅ Add support for Stack in addition to Cabal
   - ✅ Create Docker container for easier deployment
   - ✅ Implement CI/CD pipeline for automated testing

3. **Documentation**
   - Improve code documentation
   - Create comprehensive user guide
   - Add examples for common use cases

### Medium-term Goals (3-6 months)

1. **Feature Enhancements**
   - Improve image handling and rendering
   - Better support for complex tables
   - Add support for math equations and symbols
   - Implement caching for better performance

2. **User Interface**
   - Develop a modern web interface
   - Create a more user-friendly GUI application
   - Add progress indicators for long-running operations

3. **Output Formats**
   - Enhance EPUB and ODT output quality
   - Add Markdown export
   - Support direct upload to document platforms

### Long-term Goals (6+ months)

1. **Architecture Improvements**
   - Refactor codebase for better maintainability
   - Split into modular components with clear interfaces
   - Create API for integration with other applications

2. **Advanced Features**
   - Implement customizable stylesheets
   - Add collaborative editing capabilities
   - Develop a plugin system for extensibility

3. **Wider Integration**
   - Support for more wiki platforms beyond MediaWiki
   - Integration with document management systems
   - Mobile app development

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request
