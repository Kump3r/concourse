package infoserver

import (
	"net/http"

	"github.com/concourse/concourse/atc"
)

func (s *Server) Banner(w http.ResponseWriter, r *http.Request) {
	if !atc.EnableMaintenanceBanner || s.maintenanceBannerText == "" {
		w.WriteHeader(http.StatusNoContent)
		return
	}
	w.Header().Set("Content-Type", "text/plain; charset=utf-8")
	w.Write([]byte(s.maintenanceBannerText))
}
