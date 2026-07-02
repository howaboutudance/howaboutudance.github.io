/**
 * Layered hematite crystal: hematite.svg underneath, hematite.png over it
 * running the looping glitch transition. On hover the PNG fades out (and its
 * animation freezes), revealing the clean SVG. The two glitch overlay layers
 * (slice + band) live on the `.crystal-image` pseudo-elements in index.css.
 */
export function CrystalHeader() {
  return (
    <div className="crystal-image">
      <img className="z-0" src="/media/hematite.svg" alt="" aria-hidden="true" />
      <img
        className="z-[2] animate-glitch-crossfade hover:opacity-0 hover:animate-none"
        src="/media/hematite.png"
        alt="Hematite crystal"
      />
      <div className="relative z-[200] -mt-2.5 bg-black">
        <p className="font-caption text-[12pt] italic text-white">Hematite</p>
      </div>
    </div>
  )
}
