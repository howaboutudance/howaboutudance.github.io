/** Small centered copyright line. Year range ends at the current year. */
export function Footer() {
  return (
    <footer className="w-full text-center font-body text-[6pt]">
      <p className="text-white">
        &copy; 2018&ndash;{new Date().getFullYear()} Michael Penhallegon
      </p>
    </footer>
  )
}
