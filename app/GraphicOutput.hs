module GraphicOutput where
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy
import           OptimalSpeed
import           System.Directory                          (createDirectoryIfMissing)
import           System.FilePath                           (takeDirectory)


makeChart :: [(OptimalSpeed, Double)] -> (OptimalSpeed, Double) -> EC (Layout OptimalSpeed Double) ()
makeChart membershipValues crispSpeed = do
    layout_title .= "Optimal Speed = " ++ show (fst crispSpeed)
    setColors [opaque blue, opaque red]
    plot (line "mu(x)" [membershipValues])
    plot $ do
        chart <- points "Optimal speed (centroid)" [crispSpeed]
        return $ chart & (plot_points_style . point_radius .~ 3)


outputChart :: [(OptimalSpeed, Double)] -> (OptimalSpeed, Double) -> FilePath -> IO ()
outputChart membershipValues crispSpeed outputPath = do
    createDirectoryIfMissing True (takeDirectory outputPath)
    toFile def outputPath (makeChart membershipValues crispSpeed)
