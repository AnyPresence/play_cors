import com.anypresence.playframework.cors.Cors
import play.api.GlobalSettings
import play.api.mvc.WithFilters

object Global extends WithFilters(Cors.corsFilter) 