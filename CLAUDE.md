# CLAUDE.md — hematite.tech

Personal professional website for Michael Penhallegon. Single-page, static,
deployed on Cloudflare Pages. This file governs **Milestone 1**: a faithful
rebuild of the existing design on a modern stack. Resist scope creep — no
blog, no router, no CMS. That's Milestone 2's problem.

## Stack

- **Build:** Vite
- **UI:** React 19 + TypeScript (strict mode)
- **Styling:** Tailwind CSS v4 via `@tailwindcss/vite` (CSS-first config in
  `src/styles/index.css` using `@theme`; there is no `tailwind.config.js`)
- **Package manager:** npm (commit `package-lock.json`)
- **Node:** 22 LTS (pin in `.nvmrc` and Cloudflare Pages env var
  `NODE_VERSION`)
- **Branch:** `main` is the default and only long-lived branch

## Commands

```
npm install          # setup
npm run dev          # Vite dev server
npm run build        # tsc -b && vite build → dist/
npm run preview      # serve dist/ locally
npm run typecheck    # tsc -b (checks all project references; app/node are noEmit)
```

## Repository layout

```
/
├── index.html              # Vite entry (root, per Vite convention)
├── src/
│   ├── main.tsx
│   ├── App.tsx
│   ├── components/
│   │   ├── CrystalHeader.tsx   # layered hematite images + glitch animations + caption
│   │   ├── LinkList.tsx        # CV / Resume / GitHub / LinkedIn
│   │   └── Footer.tsx
│   └── styles/
│       └── index.css           # Tailwind entry + @theme tokens + @keyframes + @layer components (.crystal-image)
├── public/
│   ├── media/              # hematite.png, hematite.svg, CV.pdf,
│   │                       # resume.pdf, resume.docx — served at /media/*
│   └── <subsites>/         # see "Subsites" below
├── .nvmrc
├── package.json
└── tsconfig.json
```

## Subsites — DO NOT TOUCH

Legacy subsites live under `public/` and are copied verbatim into `dist/`
by Vite. Their URLs must not change. Rules:

- Never modify, lint, reformat, or "modernize" files inside subsite
  directories. They are frozen artifacts.
- Never let the main site's tooling (TypeScript, Tailwind, ESLint) process
  subsite directories — exclude them in configs if they complain.
- If a subsite path collides with a main-site route, the subsite wins;
  rename the main-site route instead.

## Design requirements (parity with previous site)

The visual identity is a black page with a layered hematite crystal image
and a single row of links. Reproduce it exactly, then stop.

- **Background:** near-black layered dark scheme is acceptable, but default
  to `#000` for Milestone 1 parity.
- **Crystal header:** `hematite.svg` underneath, `hematite.png` layered on
  top of it. On hover the PNG fades to opacity 0, revealing the SVG. The
  transition uses a **looping glitch effect** (not the old simple flicker)
  implemented as three coordinated keyframe animations on a 6s
  `steps(1, end) infinite` cycle with two burst windows at ~27–34% and
  ~62–68%:
    - `glitch-crossfade` — on the PNG layer: jump-cut opacity with
      `translateX/Y` jitter and `hue-rotate` during bursts
    - `glitch-slice` — on `::before` (hematite.png as overlay, `mix-blend-mode:
      screen`): torn `clip-path: inset()` slices that shift horizontally
    - `glitch-band` — on `::after`: repeating horizontal stripe gradient
      (7 px gap / 2 px band, warm red-brown at ~0.18 opacity, `mix-blend-mode:
      screen`) that shifts `background-position` and `translateX` each step
  All three animations are disabled on hover. Implementation split:
  - `@keyframes` blocks live in `src/styles/index.css` (CSS-only, no alternative)
  - Animation shorthand tokens go in the `@theme` block:
    `--animate-glitch-crossfade`, `--animate-glitch-slice`, `--animate-glitch-band`
    — Tailwind generates the utility classes from these
  - The PNG `<img>` gets animation utilities applied directly in JSX
    (`animate-glitch-crossfade hover:opacity-0 hover:animate-none`)
  - `::before` / `::after` pseudo-elements use a `.crystal-image` class in
    `@layer components` in `index.css` — clip-path, background-image, and
    mix-blend-mode are too complex for `before:`/`after:` arbitrary variants
  Layering via CSS grid or absolute positioning — do not port the old
  `margin-top: -345px` hack.
- **Caption:** the word "Hematite" beneath the image, font **IM Fell
  English** italic (note correct spelling — the old site requested
  `IM+Felol+English` from Google Fonts and silently fell back for years).
- **Body font:** Roboto 500.
- **Links row:** inline list separated by `|`, white text, `coral`
  background on hover. Items, in order:
  1. Resume → `/media/resume.pdf`, with a `(docx)` link → `/media/resume.docx`
     shown alongside it (PDF is the default; docx is the Word version)
  2. GitHub → `https://github.com/howaboutudance` with `rel="me"`
  3. LinkedIn → `https://www.linkedin.com/in/mpenhall` with `rel="me"`
  (No CV link — the `CV.pdf` file is stale; it was deliberately removed in
  commit `8c5d624` and stays unlinked. The file remains under `public/media/`
  for any existing bookmarks.)
- **Footer:** copyright line, small centered text. Update the year range.
- **`<head>`:** title `hematite.tech`; keep `rel="me"` identity links but
  fix them (https, correct domains). Load fonts via Google Fonts `<link>`
  with `preconnect`.

## Explicitly removed from the old site

Do not carry these forward:

- jQuery (was loaded, never used)
- Universal Analytics tag `UA-122609568-1` (UA was sunset in 2023; the tag
  is dead weight). If analytics are wanted later, use Cloudflare Web
  Analytics — that's a Milestone 2 decision, not yours.
- Invalid markup: `<tr>` elements inside `<ul>`, missing doctype, typo'd
  font family, `twitter.con`, single-slash `http:/` links
- the old `./r` folder since it's old code that exists elsewhere.

## Deployment — Cloudflare Pages

- Git integration on the `main` branch; every push to `main` deploys
  production, PRs get preview deployments automatically.
- **Build command:** `npm run build`
- **Build output directory:** `dist`
- **Env:** `NODE_VERSION=22`
- No GitHub Actions workflow is needed for deploys. If CI is added, limit
  it to `npm run typecheck && npm run build` as a PR check.
- Custom domain `hematite.tech` is configured in the Cloudflare Pages
  dashboard, not in the repo. Do not add a `CNAME` file — that's a GitHub
  Pages artifact.

## One-time migration checklist (delete this section when done)

- [x] `git branch -m master main && git push -u origin main`
- [x] Flip default branch to `main` on GitHub; delete `origin/master`
- [ ] Move site files into the layout above; move subsites under `public/`
- [ ] Scaffold Vite + React + TS + Tailwind; port design per requirements
- [ ] Create Cloudflare Pages project, connect repo, set build settings
- [ ] Point `hematite.tech` DNS at Cloudflare Pages; verify TLS
- [ ] Verify subsite URLs resolve identically to the old deployment
- [ ] Disable GitHub Pages on the repo

## Conventions

- TypeScript strict; no `any` without a comment justifying it.
- Components are small, function-style, one per file, named exports.
- Styling is Tailwind utilities in JSX; the only CSS file is
  `src/styles/index.css` (theme tokens + keyframes). No CSS Modules, no
  styled-components.
- Keep dependencies minimal. Adding a dependency requires a reason better
  than "it was in a tutorial."
- Assets referenced by URL live in `public/media/`; do not import binary
  assets through the bundler in Milestone 1 (keeps URLs stable for the
  resume/CV links people may have bookmarked).

## Out of scope for Milestone 1

Blog, React Router, GA4/analytics, dark/light theming, redesigned color
palette, subsite modernization.