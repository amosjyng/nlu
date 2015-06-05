(new-type {modifier} {thing})
(new-type {entity} {thing})
(new-is-a {physical object} {entity})

(new-type {article (grammar)} {thing})
(new-indv {the (article)} {article (grammar)} :english "the")
(new-indv {a (article)} {article (grammar)} :english "a")
(new-indv {an (article)} {article (grammar)} :english "an")

(new-type {kick} {action} :english '(:no-iname :verb "kick"))
(new-type {screw} {action} :english '(:no-iname :verb "screw"))
(new-type {screw in} {action} :english '(:no-iname :verb "screw in"))
(new-type {bolt} {physical object})
