{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Physics.ForceLayout
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A simple, Haskell-native simulator for doing force-directed layout,
-- /e.g./ of trees or graphs.
--
-- To use, just create an 'Ensemble' like so:
--
-- > import           Physics.ForceLayout
-- > import qualified Data.Map              as M
-- > import           Data.AffineSpace.Point
-- > import           Data.Default (def)
-- >
-- > e :: Ensemble (Double, Double)
-- > e = Ensemble [ (edges,    hookeForce 0.05 4)
-- >              , (allPairs, coulombForce 1)
-- >              ]
-- >              particleMap
-- >   where edges       = [(1,2), (2,3), (2,5), (3,5), (3,4), (4,5)]
-- >         allPairs    = [(x,y) | x <- [1..4], y <- [x+1..5]]
-- >         particleMap = M.fromList . zip [1..]
-- >                     . map (initParticle . P)
-- >                     $ [ (2.0, 3.1), (6.3, 7.2)
-- >                       , (0.3, 4.2), (1.6, -1.1)
-- >                       , (4.8, 2.9)
-- >                       ]
--
-- Then run a simulation using either 'simulate' (to get the list of
-- all intermediate states) or 'forceLayout' (to get only the ending
-- state):
--
-- > e' :: Ensemble (Double, Double)
-- > e' = forceLayout (def & damping     .~ 0.8
-- >                       & energyLimit .~ Just 0.001
-- >                       & stepLimit   .~ Nothing
-- >                  )
-- >                  e
--
-- See the diagrams-contrib package
-- (<http://github.com/diagrams/diagrams-contrib/>) for more
-- examples.
-----------------------------------------------------------------------------

module Physics.ForceLayout
       ( -- * Data structures

         Particle(..), pos, vel, force
       , initParticle

       , PID
       , Edge
       , Ensemble(..), forces, particles

         -- * Pre-defined forces

       , hookeForce
       , coulombForce
       , distForce

         -- * Running simulations

       , ForceLayoutOpts(..)
       , damping, energyLimit, stepLimit
       , simulate
       , forceLayout

         -- * Internals

       , ensembleStep
       , particleStep
       , recalcForces
       , kineticEnergy

       ) where
--import qualified Data.AffineSpace.Point as P
import           Data.Default (def)
import           Data.AffineSpace
import           Data.AffineSpace.Point
import           Data.Default.Class
import           Data.Foldable          (foldMap)
import qualified Data.Foldable          as F
import qualified Data.Map               as M
import           Data.Monoid
import           Data.VectorSpace       hiding (Sum)

import           Control.Lens

------------------------------------------------------------
--  Particles
------------------------------------------------------------

-- | A particle has a current position, current velocity, and current
--   force acting on it.
data Particle v = Particle { _pos   :: Point v
                           , _vel   :: v
                           , _force :: v
                           }
  deriving (Eq, Show)

makeLenses ''Particle

-- | Create an initial particle at rest at a particular location.
initParticle :: AdditiveGroup v => Point v -> Particle v
initParticle p = Particle p zeroV zeroV

------------------------------------------------------------
--  Ensembles
------------------------------------------------------------

-- | Used to uniquely identify particles.
type PID = Int

-- | An edge is a pair of particle identifiers.
type Edge = (PID, PID)

-- | An @Ensemble@ is a physical configuration of particles.  It
--   consists of a mapping from particle IDs (unique integers) to
--   particles, and a list of forces that are operative.  Each force
--   has a list of edges to which it applies, and is represented by a
--   function giving the force between any two points.
data Ensemble v = Ensemble { _forces    :: [([Edge], Point v -> Point v -> v)]
                           , _particles :: M.Map PID (Particle v)
                           }

makeLenses ''Ensemble

------------------------------------------------------------
--  Simulation internals
------------------------------------------------------------

-- | Simulate one time step for an entire ensemble, with the given
--   damping factor.
ensembleStep :: VectorSpace v => Scalar v -> Ensemble v -> Ensemble v
ensembleStep d = (over particles . M.map) (particleStep d) . recalcForces

-- | Simulate one time step for a particle (assuming the force acting
--   on it has already been computed), with the given damping factor.
particleStep :: VectorSpace v => Scalar v -> Particle v -> Particle v
particleStep d = stepPos . stepVel
  where stepVel p = vel .~ (d *^ (p^.vel ^+^ p^.force)) $ p
        stepPos p = pos %~ (.+^ p^.vel) $ p

-- | Recalculate all the forces acting in the next time step of an
--   ensemble.
recalcForces :: forall v. AdditiveGroup v => Ensemble v -> Ensemble v
recalcForces = calcForces . zeroForces
  where zeroForces = (particles %~) . M.map $ force .~ zeroV
        calcForces (Ensemble fs ps)
          = Ensemble fs
            (ala Endo foldMap (concatMap (\(es, f) -> (map (mkForce f) es)) fs) ps)
        mkForce :: (Point v -> Point v -> v) -> Edge -> M.Map Int (Particle v) -> M.Map Int (Particle v)
        mkForce f (i1, i2) m
          = case (M.lookup i1 m, M.lookup i2 m) of
              (Just p1, Just p2) ->
                ( M.adjust (force %~ (^+^ f (p1^.pos) (p2^.pos))) i1
                . M.adjust (force %~ (^-^ f (p1^.pos) (p2^.pos))) i2)
                m
              _                  -> m

-- | Compute the total kinetic energy of an ensemble.
kineticEnergy :: (InnerSpace v, Num (Scalar v)) => Ensemble v -> Scalar v
kineticEnergy = ala Sum F.foldMap . fmap (magnitudeSq . view vel) . view particles

------------------------------------------------------------
--  Simulation
------------------------------------------------------------

-- | Options for customizing a simulation.
data ForceLayoutOpts v =
  FLOpts
  { _damping     :: Scalar v           -- ^ Damping factor to be
                                       --   applied at each step.
                                       --   Should be between 0 and 1.
                                       --   The default is 0.8.
  , _energyLimit :: Maybe (Scalar v)   -- ^ Kinetic energy below which
                                       --   simulation should stop.
                                       --   If @Nothing@, pay no
                                       --   attention to kinetic
                                       --   energy.  The default is
                                       --   @Just 0.001@.
  , _stepLimit   :: Maybe Int          -- ^ Maximum number of
                                       --   simulation steps.  If
                                       --   @Nothing@, pay no
                                       --   attention to the number of
                                       --   steps.  The default is
                                       --   @Just 1000@.
  }

makeLenses ''ForceLayoutOpts

instance Fractional (Scalar v) => Default (ForceLayoutOpts v) where
  def = FLOpts
        { _damping     = 0.8
        , _energyLimit = Just 0.001
        , _stepLimit   = Just 1000
        }

-- | Simulate a starting ensemble according to the given options,
--   producing a list of all the intermediate ensembles.  Useful for,
--   /e.g./, making an animation.  Note that the resulting list could
--   be infinite, if a 'stepLimit' is not specified and either the
--   kinetic energy never falls below the specified threshold, or no
--   energy threshold is specified.
simulate :: (InnerSpace v, Ord (Scalar v), Num (Scalar v))
         => ForceLayoutOpts v -> Ensemble v -> [Ensemble v]
simulate opts e
  = (e:)
  . takeWhile (maybe (const True) (<) (opts ^. energyLimit) . kineticEnergy)
  . maybe id take (opts ^. stepLimit)
  . drop 1
  . iterate (ensembleStep (opts ^. damping))
  $ e

-- | Run a simluation from a starting ensemble, yielding either the
--   first ensemble to have kinetic energy below the 'energyLimit' (if
--   given), or the ensemble that results after a number of steps
--   equal to the 'stepLimit' (if given), whichever comes first.
--   Otherwise @forceLayout@ will not terminate.
forceLayout :: (InnerSpace v, Ord (Scalar v), Num (Scalar v))
            => ForceLayoutOpts v -> Ensemble v -> Ensemble v
forceLayout opts = last . simulate opts

------------------------------------------------------------
--  Standard forces
------------------------------------------------------------

-- | @distForce f p1 p2@ computes the force between two points as a
--   multiple of the unit vector in the direction from @p1@ to @p2@,
--   given a function @f@ which computes the force's magnitude as a
--   function of the distance between the points.
distForce :: (InnerSpace v, Floating (Scalar v))
          => (Scalar v -> Scalar v) -> Point v -> Point v -> v
distForce f p1 p2 = withLength (f (distance p1 p2)) (p2 .-. p1)
  where withLength s v = s *^ normalized v

-- | @hookeForce k l p1 p2@ computes the force on @p1@, assuming that
--   @p1@ and @p2@ are connected by a spring with equilibrium length @l@
--   and spring constant @k@.
hookeForce :: (InnerSpace v, Floating (Scalar v))
           => Scalar v -> Scalar v -> Point v -> Point v -> v
hookeForce k l = distForce (\d -> k * (d - l))

-- | @coulombForce k@ computes the electrostatic repulsive force
--   between two charged particles, with constant of proportionality
--   @k@.
coulombForce :: (InnerSpace v, Floating (Scalar v))
             => Scalar v -> Point v -> Point v -> v
coulombForce k = distForce (\d -> -k * 1/(d*d))


e :: Ensemble (Double, Double)
e = Ensemble [ (edges,    hookeForce 0.05 4)
             , (allPairs, coulombForce 1)
             ]
             particleMap
  where edges       = [(1,2), (2,3), (2,5), (3,5), (3,4), (4,5)]
        allPairs    = [(x,y) | x <- [1..4], y <- [x+1..5]]
        particleMap = M.fromList . zip [1..]
                    . map (initParticle . P)
                    $ [ (2.0, 3.1), (6.3, 7.2)
                      , (0.3, 4.2), (1.6, -1.1)
                      , (4.8, 2.9)
                      ]
e' = forceLayout (FLOpts { _damping     = 0.8
                         , _energyLimit = Just 0.001
                         , _stepLimit   = Nothing
                         }
                 )
                 e
