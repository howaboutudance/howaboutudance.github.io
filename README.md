# hematite.tech

Personal professional website for Michael Penhallegon — a single-page static
site. Black page, a layered hematite crystal image with a looping glitch
transition, and a row of links.

Built with **Vite + React 19 + TypeScript + Tailwind CSS v4**, deployed on
**Cloudflare Pages**.

## Prerequisites

- **Node 22 LTS** (pinned in `.nvmrc`). With [nvm](https://github.com/nvm-sh/nvm):
  ```sh
  nvm use      # or `nvm install` the first time
  ```
- **npm** (comes with Node; the repo commits `package-lock.json`).

## Getting started

```sh
git clone git@github.com:howaboutudance/howaboutudance.github.io.git
cd howaboutudance.github.io
nvm use
npm install
npm run dev
```

`npm run dev` starts the Vite dev server with hot-reload at
**http://localhost:5173**.

## Scripts

| Command             | What it does                                             |
| ------------------- | -------------------------------------------------------- |
| `npm run dev`       | Vite dev server with hot module reload                   |
| `npm run build`     | `tsc -b && vite build` → production output in `dist/`    |
| `npm run preview`   | Serve the built `dist/` locally to sanity-check a build  |
| `npm run typecheck` | `tsc -b` — typecheck all project references (no emit)    |

## Project layout

```
/
├── index.html              # Vite entry (fonts, rel=me links, #root)
├── src/
│   ├── main.tsx            # React entry
│   ├── App.tsx
│   ├── components/
│   │   ├── CrystalHeader.tsx   # layered hematite images + glitch animation + caption
│   │   ├── LinkList.tsx        # Resume / (docx) / GitHub / LinkedIn
│   │   └── Footer.tsx
│   └── styles/
│       └── index.css       # Tailwind entry + @theme tokens + @keyframes + .crystal-image
├── public/                 # copied verbatim into dist/
│   ├── media/              # hematite.png/.svg, resume.pdf/.docx, CV.pdf — served at /media/*
│   └── <subsites>/         # frozen legacy pages (see below)
├── .nvmrc                  # Node 22
├── vite.config.ts
├── tsconfig*.json
└── package.json
```

### Legacy subsites — do not modify

Directories under `public/` (e.g. `NMSPcheckin/`, `graduation/`) are frozen
legacy pages copied verbatim into `dist/`. **Do not edit, lint, or reformat
them** — their URLs must not change. Keep the main site's tooling
(TypeScript/Tailwind/ESLint) from processing them.

## Styling

Tailwind CSS v4 with a CSS-first config — there is **no `tailwind.config.js`**.
Theme tokens (colors, fonts, animation shorthands) live in the `@theme` block
in `src/styles/index.css`, which is also home to the glitch `@keyframes` and
the `.crystal-image` component styles. Everything else is Tailwind utility
classes in JSX.

## Deployment — Cloudflare Pages

Deployed via Cloudflare Pages Git integration (not GitHub Actions):

- **Build command:** `npm run build`
- **Build output directory:** `dist`
- **Environment:** `NODE_VERSION=22`

Pushes to `main` deploy to production; pull requests get preview deployments.
The custom domain `hematite.tech` is configured in the Cloudflare Pages
dashboard — do **not** add a `CNAME` file (that's a GitHub Pages artifact).
