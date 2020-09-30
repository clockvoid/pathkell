module Camera where

import Base.Vec
import Base.Ray

data Camera = Camera Vec3 (Vec3, Vec3, Vec3)

camera :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Camera
camera m_origin m_u m_v m_w = Camera m_origin (m_u, m_v, m_w)

cameraFromLookAt :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Camera
cameraFromLookAt lookfrom lookat vup vfov aspect = camera m_origin (2 * halfW |* u) (2 * halfH |* v) (m_origin - halfW |* u - halfH |* v - w)
  where
    halfH = tan (radians vfov) / 2.0
    halfW = aspect * halfH
    m_origin = vec3 0 0 0
    w = normalize $ lookfrom - lookat
    u = normalize $ cross vup w
    v = cross w u

getRay :: Double -> Double -> Camera -> Ray
getRay u v (Camera m_origin (m_u, m_v, m_w)) = Ray m_origin (m_w + (m_u *| u) + (m_v *| v) - m_origin)

