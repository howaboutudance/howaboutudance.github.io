/**
 * Inline row of links separated by `|`, white text with a coral hover
 * background. "Resume" links to the PDF; the "(docx)" beside it links to the
 * Word version.
 *
 * (CV was intentionally dropped — the file is stale; see commit history.)
 */
export function LinkList() {
  return (
    <ul className="mt-4 [&>li]:inline-block [&>li+li]:before:px-[0.2em] [&>li+li]:before:text-white [&>li+li]:before:content-['|']">
      <li>
        <a className="text-white hover:bg-coral" href="/media/resume.pdf">
          Resume
        </a>
        <a className="ml-1 text-white hover:bg-coral" href="/media/resume.docx">
          (docx)
        </a>
      </li>
      <li>
        <a
          className="text-white hover:bg-coral"
          href="https://github.com/howaboutudance"
          rel="me"
        >
          GitHub
        </a>
      </li>
      <li>
        <a
          className="text-white hover:bg-coral"
          href="https://www.linkedin.com/in/mpenhall"
          rel="me"
        >
          LinkedIn
        </a>
      </li>
    </ul>
  )
}
