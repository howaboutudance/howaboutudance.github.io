import { CrystalHeader } from './components/CrystalHeader'
import { LinkList } from './components/LinkList'
import { Footer } from './components/Footer'

export function App() {
  return (
    <>
      <article className="mt-32 w-full text-center font-body">
        <CrystalHeader />
        <LinkList />
      </article>
      <Footer />
    </>
  )
}
