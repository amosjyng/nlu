(new-type {modifier} {thing})
(new-type {entity} {thing})
(new-is-a {physical object} {entity})
(new-is-a {tangible} {physical object})

(new-type {article (grammar)} {thing})
(new-indv {the (article)} {article (grammar)} :english "the")
(new-indv {a (article)} {article (grammar)} :english "a")
(new-indv {an (article)} {article (grammar)} :english "an")

(new-type {colored thing} {tangible} :english '(:no-iname :adj "colored"))
(new-is-a {colored thing} {modifier})
(new-type {red thing} {colored thing} :english '(:no-iname :adj "red"))
(new-type {sizable thing} {tangible} :english :no-iname)
(new-is-a {sizable thing} {modifier})
(new-type {big thing} {sizable thing} :english '(:no-iname :adj "big" "large"))

(new-type {screw (fastener)} {tangible} :english '(:no-iname "screw"))
(new-type {bolt} {screw (fastener)})
(new-type {tool} {tangible})
(new-type {screwdriver} {tool})

(new-type {kick} {action} :english :verb)
(new-type {screw} {action} :english :verb)
(new-type {choose} {action} :english '(:verb "choose" "pick"))
(new-type {screw in} {action} :english :verb)
(new-type {pick up} {action} :english :verb)
