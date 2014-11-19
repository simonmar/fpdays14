module Blog (blog) where

import Types
import BlogDataSource

import Haxl.Prelude

import Data.List
import Data.Ord
import qualified Data.Map as Map
import Data.Map (Map)
import Prelude ()

-- -----------------------------------------------------------------------------
-- Blog code

blog :: Haxl Html
blog = renderPage <$> leftPane <*> mainPane

getAllPostsInfo :: Haxl [PostInfo]
getAllPostsInfo = do
  ids <- getPostIds
  mapM getPostInfo ids

getPostDetails :: PostId -> Haxl (PostInfo, PostContent)
getPostDetails pid = pair (getPostInfo pid) (getPostContent pid)

leftPane :: Haxl Html
leftPane = renderSidePane <$> popularPosts <*> topics

topics :: Haxl Html
topics = do
  posts <- getAllPostsInfo
  let topiccounts =
        Map.fromListWith (+)
          [ (postTopic p, 1) | p <- posts ]
  return $ renderTopics topiccounts

popularPosts :: Haxl Html
popularPosts = do
  pids <- getPostIds
  views <- mapM getPostViews pids
  let ordered =
        take 5 $ map fst $
        sortBy (flip (comparing snd))
               (zip pids views)
  content <- mapM getPostDetails ordered
  return $ renderPostList content

mainPane :: Haxl Html
mainPane = do
  posts <- getAllPostsInfo
  let ordered =
        take 5 $
        sortBy (flip (comparing postDate)) posts
  content <- mapM (getPostContent . postId) ordered
  return $ renderPosts (zip ordered content)

-- -----------------------------------------------------------------------------
-- Dummy rendering

data Html = Html
  deriving Show

renderPage :: Html -> Html -> Html
renderPage _ _ = Html

renderPosts :: [(PostInfo, PostContent)] -> Html
renderPosts _ = Html

renderSidePane :: Html -> Html -> Html
renderSidePane _ _ = Html

renderPostList :: [(PostInfo, PostContent)] -> Html
renderPostList _ = Html

renderTopics :: Map String Int -> Html
renderTopics _ = Html
