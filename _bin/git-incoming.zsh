#!/bin/zsh

emulate zsh

CURRENT_BRANCH="$(git branch | grep '^*' | sed s/\*\ //)"

if [[ "${CURRENT_BRANCH}" != "" ]] {
    TRACKED_REPOSITORY="$(git config branch.${CURRENT_BRANCH}.remote)"

    if [[ "${TRACKED_REPOSITORY}" != "" ]] {
        REMOTE_BRANCH="$(git config branch.${CURRENT_BRANCH}.merge | cut -d"/" -f3)"

        if [[ "${REMOTE_BRANCH}" != "" ]] {
            TARGET="${TRACKED_REPOSITORY}/${REMOTE_BRANCH}"
            git fetch ${TRACKED_REPOSITORY} ${REMOTE_BRANCH}

            git log ..${TARGET}
        } else {
              echo "Current branch has no corresponding remote repository."
              echo 'Try setting branch.$CurrentBranch.merge'
          }
    } else {
          echo "Current branch doesn't track any repository."
          echo 'Try setting branch.$CurrentBranch.remote'
        }
}
